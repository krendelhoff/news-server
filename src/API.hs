{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns #-}
module API where

import           Control.Applicative
import           Control.Exception
import qualified Data.Text           as T
import           Data.Time
import           GHC.TypeLits
import           Network.HTTP.Types
import           Network.Wai         hiding (Request)
import           TextShow
import           Universum
import Network.Wai.Handler.Warp

data Verb (m :: StdMethod) (a :: Type)

type Get a = Verb 'GET a

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: Type)
infixr 9 :>

data Capture (a :: Type)

--type MyAPI = "date" :> Get Day :<|> "time" :> Capture TimeZone

type family Server (a :: Type) :: Type
type instance Server (Get a) = IO a
type instance Server (a :<|> b) = Server a :<|> Server b
type instance Server ((s :: Symbol) :> r) = Server r
type instance Server (Capture a :> r) = a -> Server r

error :: Response
error = responseLBS status404 [] "404!"

type Request = [Text]

class HasServer layout where
  route :: Server layout -> Request -> Maybe (IO Response)

instance TextShow a => HasServer (Get a) where
  route :: IO a -> Request -> Maybe (IO Response)
  route handler _ = return do
    handler <&> responseLBS status200 [] . encodeUtf8 . showt

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route :: Server a :<|> Server b -> Request -> Maybe (IO Response)
  route (handler1 :<|> handler2) req = route @a handler1 req
                                   <|> route @b handler2 req

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route :: Server r -> Request -> Maybe (IO Response)
  route h (x:xs) | symbolVal (Proxy @s) == T.unpack x = route @r h xs
  route _ _ = Nothing

-- move from read to FromHttpApiData
instance (Read a, HasServer r) => HasServer (Capture a :> r) where
  route :: (a -> Server r) -> Request -> Maybe (IO Response)
  route handler ((T.unpack -> x):xs) = do
    a <- readMaybe x
    route @r (handler a) xs
  route _ _ = Nothing

serve :: forall layout. HasServer layout => Server layout -> Application
serve s req respond = let path = fst . decodePath $ rawPathInfo req
                        in case route @layout s path of
                            Nothing -> respond API.error
                            Just m  -> m >>= respond
