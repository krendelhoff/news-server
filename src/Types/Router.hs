{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Types.Router where

import Control.Lens
import Control.Monad.Except
import GHC.TypeLits
import Network.HTTP.Types
import Universum

import Types.Environment

data ServerError = WrongPath | CriticalError deriving (Eq, Show, Exception)

instance Semigroup ServerError where
  WrongPath <> WrongPath = WrongPath
  _ <> _                 = CriticalError

instance Monoid ServerError where
  mempty = WrongPath

newtype Handler a = Handler
  { runHandler :: ReaderT Environment (ExceptT ServerError IO) a }
  deriving ( Functor, Applicative, Monad
           , MonadError ServerError, Alternative
           , MonadReader Environment, MonadThrow, MonadCatch
           , MonadIO )
  via (ReaderT Environment (ExceptT ServerError IO))

data RequestInfo = RequestInfo { _path     :: [Text]
                               , _method   :: StdMethod
                               , _queryStr :: Query
                               , _headers  :: RequestHeaders
                               , _body     :: ByteString
                               } deriving (Eq, Show)
makeLenses ''RequestInfo

data SMethod (m :: StdMethod) where
  SPut    :: SMethod 'PUT
  SPost   :: SMethod 'POST
  SGet    :: SMethod 'GET
  SDelete :: SMethod 'DELETE

class KnownMethod (m :: StdMethod) where
  methodVal :: StdMethod

--TODO TH CREATION
instance KnownMethod 'PUT where
  methodVal = PUT

instance KnownMethod 'POST where
  methodVal = POST

instance KnownMethod 'GET where
  methodVal = GET

instance KnownMethod 'DELETE where
  methodVal = DELETE

data Verb (m :: StdMethod) (code :: Nat) (a :: Type)

type Get a    = Verb 'GET 200 a
type Put a    = Verb 'PUT 200 a
type Post a   = Verb 'POST 200 a
type Delete a = Verb 'DELETE 200 a

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: Type)
infixr 9 :>

data Capture (a :: Type)
data QueryParam (s :: Symbol) (a :: Type)
data QueryParams (s :: Symbol) (a :: Type)
data ReqBody (a :: Type)

type family Server (a :: Type) :: Type
type instance Server (Verb m c a) = Handler a
type instance Server (a :<|> b) = Server a :<|> Server b
type instance Server ((s :: Symbol) :> r) = Server r
type instance Server (Capture a :> r) = a -> Server r
type instance Server (QueryParam s a :> r) = Maybe a -> Server r
type instance Server (QueryParams s a :> r) = Vector a -> Server r
type instance Server (ReqBody a :> r) = a -> Server r
