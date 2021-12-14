{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Types.Router where

import Control.Lens
import Control.Monad.Except
import GHC.TypeLits
import Network.HTTP.Types
import Universum

import Data.Aeson
import Types.Environment

data ServerError = WrongPath | ServerError Status Message
  deriving (Eq, Show, Exception)

mkError :: Status -> Message -> ServerError
mkError = ServerError

err404 :: ServerError
err404 = ServerError status404 "Not found"

err500 :: ServerError
err500 = ServerError status500 "Internal Error"

err401 :: ServerError
err401 = ServerError status401 "Unauthorized"

err403TokenExpired :: ServerError
err403TokenExpired = ServerError status401 "Token expired"

err403TokenInvalid :: ServerError
err403TokenInvalid = ServerError status401 "Token invalid"

data TokenError = NoToken | BadToken

newtype Message = Message { message :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
  deriving newtype (IsString, Semigroup, Monoid)

newtype Handler a = Handler
  { runHandler :: ReaderT Environment (ExceptT ServerError IO) a }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadError ServerError, MonadReader Environment
                   , MonadThrow, MonadCatch, MonadIO )

data RequestInfo = RequestInfo { _path     :: [Text]
                               , _method   :: StdMethod
                               , _queryStr :: Query
                               , _headers  :: RequestHeaders
                               , _body     :: ByteString
                               , _auth     :: Bool
                               } deriving (Eq, Show)
makeLenses ''RequestInfo

data SMethod (m :: StdMethod) where
  SPut    :: SMethod 'PUT
  SPost   :: SMethod 'POST
  SGet    :: SMethod 'GET
  SDelete :: SMethod 'DELETE

-- makeKnown "StdMethod"
class KnownMethod (m :: StdMethod) where
  methodVal :: StdMethod
-- use danil's hkd for update methods
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

data Capture (id :: Symbol) (a :: Type)

data QueryParam (s :: Symbol) (a :: Type)

data QueryParams (s :: Symbol) (a :: Type)

data ReqBody (a :: Type)

data RequireAdmin
