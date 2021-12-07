{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Types.DB ( HasPool(..)
                , ConnectionString
                , HasDbConfig(..)
                , DbConfig(DbConfig)
                ) where

import Control.Lens.TH
import Hasql.Connection
import Hasql.Migration
import Hasql.Pool       as Pool

import Universum

-- dummy record for typeclass creation
newtype DBPool = DBPool { _pool :: Pool }
makeFieldsNoPrefix ''DBPool

-- TODO TH CREATION
newtype HostName = HostName Text deriving stock (Eq, Show)
                                 deriving newtype (IsString)

newtype Port = Port Word16 deriving stock (Eq, Show, Ord)
                           deriving newtype (Num)

data DbConfig = DbConfig { _dbConfigHostName :: HostName
                         , _dbConfigPort     :: Port
                         , _dbConfigUser     :: Text -- FIXME SPECIALIZE
                         , _dbConfigDbname   :: Text
                         , _dbConfigPassword :: Text
                         } deriving (Eq, Show)
makeClassy ''DbConfig

type ConnectionString = ByteString

instance Exception ConnectionError
instance Exception MigrationError
instance Exception Pool.UsageError
