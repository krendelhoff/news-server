{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
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
                , dbConfigDecoder
                ) where

import Control.Lens.TH
import Hasql.Connection
import Hasql.Migration
import Hasql.Pool       as Pool
import Dhall
import Universum

-- dummy record for typeclass creation
newtype DBPool = DBPool { _pool :: Pool }
makeFieldsNoPrefix ''DBPool

-- TODO TH CREATION
newtype HostName = HostName Text deriving stock (Eq, Show)
                                 deriving newtype (IsString)

newtype Port = Port Word16 deriving stock (Eq, Show, Ord)
                           deriving newtype (Num)

data DbConfig = DbConfig { _hostName :: Text
                         , _port     :: Text
                         , _user     :: Text -- FIXME SPECIALIZE
                         , _dbname   :: Text
                         , _password :: Text
                         } deriving (Generic, Eq, Show)
makeClassy ''DbConfig

dbConfigDecoder :: Decoder DbConfig
dbConfigDecoder = record
                    ( DbConfig <$> field "hostName" strictText
                               <*> field "port" strictText
                               <*> field "user" strictText
                               <*> field "dbname" strictText
                               <*> field "password" strictText
                    )

type ConnectionString = ByteString

instance Exception ConnectionError
instance Exception MigrationError
instance Exception Pool.UsageError
