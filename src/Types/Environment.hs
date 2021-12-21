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

import Types.Common
import Logger

import qualified Types.Users as Users

data Environment = Environment
  { _environmentPool        :: HaSQL.Pool
  , _environmentEnvDbConfig :: DbConfig
  , _environmentUserId      :: Users.ID
  , _environmentLogger      :: Logger
  }
makeFields ''Environment

instance HasDbConfig Environment where
  dbConfig = envDbConfig
