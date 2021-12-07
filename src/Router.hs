{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Router where

import Control.Applicative
import Control.Monad.Except
import Data.Time
import GHC.TypeLits
import Network.HTTP.Types
import Network.Wai              hiding (Request)
import Network.Wai.Handler.Warp
import TextShow
import Universum                hiding (error)
import Web.HttpApiData

import qualified Data.Text as T

data Verb (m :: StdMethod) (a :: Type)

type Get a = Verb 'GET a

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: Type)
infixr 9 :>

data Capture (a :: Type)

--type MyAPI = "date" :> Get Day :<|> "time" :> Capture TimeZone

type family Server (a :: Type) :: Type
type instance Server (Get a) = Handler a
type instance Server (a :<|> b) = Server a :<|> Server b
type instance Server ((s :: Symbol) :> r) = Server r
type instance Server (Capture a :> r) = a -> Server r

error :: Response
error = responseLBS status404 [] "404!"

data ServerError = U | E deriving (Eq, Show)

instance Semigroup ServerError where
  U <> U = U
  _ <> _ = E

instance Monoid ServerError where
  mempty = U

newtype Handler a = Handler { runHandler :: ExceptT ServerError IO a }
  deriving (Functor, Applicative, Monad, MonadError ServerError, Alternative)
  via (ExceptT ServerError IO)

type Request = [Text]

class HasServer layout where
  route :: Server layout -> Request -> Handler Response

instance TextShow a => HasServer (Get a) where
  route :: Handler a -> Request -> Handler Response
  route handler _ = handler <&> responseLBS status200 [] . encodeUtf8 . showt

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route :: Server a :<|> Server b -> Request -> Handler Response
  route (handler1 :<|> handler2) req = route @a handler1 req
                                   <|> route @b handler2 req

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route :: Server r -> Request -> Handler Response
  route h (x:xs) | symbolVal (Proxy @s) == T.unpack x = route @r h xs
  route _ _ = throwError E

-- move from read to FromHttpApiData
instance (FromHttpApiData a, HasServer r) => HasServer (Capture a :> r) where
  route :: (a -> Server r) -> Request -> Handler Response
  route handler (x:xs) = case parseUrlPiece x of
    Left err -> throwError U
    Right a  -> route @r (handler a) xs
  route _ _ = throwError E

serve :: forall layout. HasServer layout => Server layout -> Application
serve s req respond = do
  let path = fst . decodePath $ rawPathInfo req
  runExceptT (runHandler (route @layout s path))
    >>= either (const $ respond error) respond

