{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Environment where

import Control.Lens.TH      (makeFields)
import Control.Monad.Except (MonadError)
import Universum

import qualified Hasql.Pool as HaSQL

import Application
    ( HasAuth(..)
    , HasLogging(..)
    , HasPersistPicture(..)
    , HasPersistUser(..)
    , HasUtils(..)
    )
import Infrastructure

import qualified Application
import qualified Application.Auth     as Auth
import qualified Application.Logging  as Logging
import qualified Application.Pictures as Pictures
import qualified Application.Users    as Users
import qualified Application.Utils    as Utils


data Environment m = Environment
  { _environmentPool        :: HaSQL.Pool
  , _environmentEnvDbConfig :: DbConfig
  , _environmentApplication :: Application.Handle m
  }
makeFields ''Environment

instance HasDbConfig (Environment m) where
  dbConfig = envDbConfig

newtype AppM env m a = AppM { runAppM :: ReaderT env m a }
  deriving newtype ( Functor, Applicative, Monad, MonadReader env, MonadTrans )

deriving newtype instance MonadThrow m => MonadThrow (AppM (Environment m) m)
deriving newtype instance MonadIO m => MonadIO (AppM (Environment m) m)
deriving newtype instance MonadError ServerError m =>
  MonadError ServerError (AppM (Environment m) m)

type App = AppM (Environment Handler) Handler

type Server api = ServerT api App

runApp :: r -> AppM r m a -> m a
runApp env = flip runReaderT env . runAppM

instance Monad m => HasPersistUser (Environment m) (Users.Handle m) where
  persistUser = application . persistUser

instance Monad m => HasPersistPicture (Environment m) (Pictures.Handle m) where
  persistPicture = application . persistPicture

instance Monad m => HasLogging (Environment m) (Logging.Handle m) where
  logging = application . logging

instance Monad m => HasAuth (Environment m) (Auth.Handle m) where
  auth = application . auth

instance Monad m => HasUtils (Environment m) (Utils.Handle m) where
  utils = application . utils
