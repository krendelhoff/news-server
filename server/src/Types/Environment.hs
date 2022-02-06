{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Environment where

import Control.Lens.TH      (makeFields, makeFieldsNoPrefix)
import Dhall (FromDhall)
import Control.Monad.Except (MonadError)
import Universum

import qualified Hasql.Pool as HaSQL

import Application
import Infrastructure
import Types.Auth (Auth)
import Types.Logger (Level)

import qualified Types.Users as Users

import qualified Application.Authors    as Authors
import qualified Application.Categories as Categories
import qualified Application.Logging    as Logging
import qualified Application.Pictures   as Pictures
import qualified Application.Users      as Users
import qualified Application.Utils      as Utils

data Config = Config
  { _dbConfig       :: DbConfig
  , _dbPoolSettings :: DbPoolSettings
  , _logLevel       :: Level
  , _port           :: Port
  } deriving (Eq, Show, Generic, FromDhall)
makeFieldsNoPrefix ''Config

data Environment m = Environment
  { _pool        :: HaSQL.Pool
  , _config      :: Config
  , _application :: Application.Handle m
  }
makeFieldsNoPrefix ''Environment

newtype AppM env m a = AppM { runAppM :: ReaderT env m a }
  deriving newtype ( Functor, Applicative, Monad, MonadReader env, MonadTrans )

deriving newtype instance MonadThrow m => MonadThrow (AppM (Environment m) m)
deriving newtype instance MonadIO m => MonadIO (AppM (Environment m) m)
deriving newtype instance MonadError ServerError m =>
  MonadError ServerError (AppM (Environment m) m)

type App = AppM (Environment Handler) Handler

newtype AuthenticatedApp (rights :: [Auth]) a =
  AuthenticatedApp { runAuthApp :: ReaderT Users.ID App a }
  deriving newtype ( Functor, Applicative, Monad, MonadReader Users.ID
                   , MonadError ServerError, MonadIO, MonadThrow
                   )

runApp :: r -> AppM r m a -> m a
runApp env = flip runReaderT env . runAppM

instance Monad m => HasPersistUser (Environment m) (Users.Handle m) where
  persistUser = application . persistUser

instance Monad m => HasPersistPicture (Environment m) (Pictures.Handle m) where
  persistPicture = application . persistPicture

instance Monad m => HasLogging (Environment m) (Logging.Handle m) where
  logging = application . logging

instance Monad m => HasUtils (Environment m) (Utils.Handle m) where
  utils = application . utils

instance Monad m => HasPersistAuthor (Environment m) (Authors.Handle m) where
  persistAuthor = application . persistAuthor

instance Monad m => HasPersistCategories (Environment m) (Categories.Handle m) where
  persistCategories = application . persistCategories
