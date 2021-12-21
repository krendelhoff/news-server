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
{-# LANGUAGE OverloadedStrings          #-}
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
import Common

newtype Handler a = Handler
  { runHandler :: ReaderT Environment (ExceptT ServerError IO) a }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadError ServerError, MonadReader Environment
                   , MonadThrow, MonadCatch, MonadIO
                   )

data AuthLevel = NoAuth | RequireUser | RequireAdmin deriving (Eq, Show)

instance Semigroup AuthLevel where
  NoAuth <> x                = x
  x <> NoAuth                = x
  RequireAdmin <> _          = RequireAdmin
  _ <> RequireAdmin          = RequireAdmin
  RequireUser <> RequireUser = RequireUser

instance Monoid AuthLevel where
  mempty = NoAuth

data RequestInfo = RequestInfo { _path     :: [Text]
                               , _method   :: StdMethod
                               , _queryStr :: Query
                               , _headers  :: RequestHeaders
                               , _body     :: ByteString
                               , _auth     :: AuthLevel
                               } deriving (Eq, Show)
makeLenses ''RequestInfo

data SMethod (m :: StdMethod) where
  SPut    :: SMethod 'PUT
  SPost   :: SMethod 'POST
  SGet    :: SMethod 'GET
  SDelete :: SMethod 'DELETE

makeKnown' (stripModifier "Std") ''StdMethod

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
