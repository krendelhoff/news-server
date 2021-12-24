{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types.Environment where

import Control.Lens.TH
import Universum

import qualified Hasql.Pool as HaSQL

import Types.DB
import Types.Lenses
import Types.Users
import Logger

import qualified Types.Users as Users
import qualified Application

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
