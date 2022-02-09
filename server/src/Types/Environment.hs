{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Environment where

import Control.Lens.TH      (makeFields, makeFieldsNoPrefix)
import Control.Monad.Except (MonadError)
import Data.Aeson           (FromJSON)
import Deriving.Aeson
import Universum

import qualified Hasql.Pool as HaSQL

import Infrastructure
import Types.Auth     (Rights)
import Types.Logger   (HasLogger(logger), Level, Logger(Logger))

import qualified Types.Users as Users

data Config = Config
  { _dbConfig       :: DbConfig
  , _dbPoolSettings :: DbPoolSettings
  , _doLogging      :: Bool
  , _logLevel       :: Level
  , _port           :: Port
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON) via
    (CustomJSON '[FieldLabelModifier '[StripPrefix "_"]] Config)
makeFieldsNoPrefix ''Config

data Environment = Environment
  { _pool   :: HaSQL.Pool
  , _config :: Config
  , _logger :: Logger
  }
makeFieldsNoPrefix ''Environment

newtype AppM env m a = AppM { runAppM :: ReaderT env m a }
  deriving newtype ( Functor, Applicative, Monad, MonadReader env, MonadTrans )

deriving newtype instance MonadThrow m => MonadThrow (AppM Environment m)
deriving newtype instance MonadIO m => MonadIO (AppM Environment m)
deriving newtype instance MonadError ServerError m =>
  MonadError ServerError (AppM Environment m)

type App = AppM Environment Handler

newtype AuthenticatedApp (rights :: [Rights]) a =
  AuthenticatedApp { runAuthApp :: ReaderT Users.ID App a }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadError ServerError, MonadIO, MonadThrow
                   )

class HasUserId (m :: Type -> Type) where
  getUserId :: m Users.ID

instance HasUserId (AuthenticatedApp rights) where
  getUserId = AuthenticatedApp ask

instance MonadReader Environment (AuthenticatedApp rights) where
  ask = AuthenticatedApp $ lift ask
  local f m =
    getUserId >>= AuthenticatedApp . lift . local f . runReaderT (runAuthApp m)

runApp :: r -> AppM r m a -> m a
runApp env = flip runReaderT env . runAppM
