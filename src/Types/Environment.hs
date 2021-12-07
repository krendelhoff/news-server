{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types.Environment where

import Control.Lens.TH
import Types.DB
import Universum

import qualified Hasql.Pool as HaSQL

data Environment = Environment
  { _environmentPool        :: HaSQL.Pool
  , _environmentEnvDbConfig :: DbConfig
  }
makeFields ''Environment

instance HasDbConfig Environment where
  dbConfig = envDbConfig
