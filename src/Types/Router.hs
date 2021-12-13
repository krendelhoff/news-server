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
{-# OPTIONS_GHC -Wno-deriving-defaults  #-}
module Types.Router where

import Control.Lens
import Control.Monad.Except
import GHC.TypeLits
import Network.HTTP.Types
import Universum

import Data.Aeson
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
                               , _auth     :: Bool
                               } deriving (Eq, Show)
makeLenses ''RequestInfo

newtype Message = Message { message :: Text }
  deriving (Eq, Show, Generic, ToJSON)
  deriving newtype (IsString)

data SMethod (m :: StdMethod) where
  SPut    :: SMethod 'PUT
  SPost   :: SMethod 'POST
  SGet    :: SMethod 'GET
  SDelete :: SMethod 'DELETE

-- makeKnown "StdMethod"
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

data RequireAdmin
