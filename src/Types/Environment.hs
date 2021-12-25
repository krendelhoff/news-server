{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Types.Environment where

import Control.Lens.TH
import Control.Monad.Except (MonadError)
import Universum

import qualified Hasql.Pool as HaSQL

import Application  (HasPersistPicture(..), HasPersistUser(..), HasLogging(..))
import Logger
import Types.DB
import Types.Errors
import Errors
import Types.Lenses
import Types.Users

import qualified Application
import qualified Application.Pictures as Pictures
import qualified Application.Users    as Users
import qualified Types.Users          as Users
import qualified Application.Logging as Logging


data Environment m = Environment
  { _environmentPool        :: HaSQL.Pool
  , _environmentEnvDbConfig :: DbConfig
  , _environmentUserId      :: Users.ID
  , _environmentApplication :: Application.Handle m
  , _environmentLogger      :: Logger
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

instance Monad m => HasPersistUser (Environment m) (Users.Handle m) where
  persistUser = application . persistUser

instance Monad m => HasPersistPicture (Environment m) (Pictures.Handle m) where
  persistPicture = application . persistPicture

instance Monad m => HasLogging (Environment m) (Logging.Handle m) where
  logging = application . logging
