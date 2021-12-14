{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types.Environment where

import Control.Lens.TH
import Universum

import qualified Hasql.Pool as HaSQL

import Types.DB

import qualified Types.Users as Users

data Environment = Environment
  { _environmentPool        :: HaSQL.Pool
  , _environmentEnvDbConfig :: DbConfig
  , _environmentUserId      :: Users.ID
  }
makeFields ''Environment

instance HasDbConfig Environment where
  dbConfig = envDbConfig
