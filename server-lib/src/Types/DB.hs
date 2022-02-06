{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Types.DB where

import Control.Lens.TH (makeFieldsNoPrefix)
import Dhall
    ( Decoder
    , FromDhall
    , Generic
    , Text
    , field
    , record
    , strictText
    )
import Universum

import Data.Time (NominalDiffTime)
import Types.TH

newIntType "PoolSize"
newIntType "ConnTimeout"
newTextType "HostName"
newTextType "DbUser"
newTextType "DbName"
newTextType "DbPassword"

newtype Port = Port Word16 deriving stock (Eq, Show, Generic)
                           deriving newtype (Ord, Num, FromDhall)

data DbPoolSettings = DbPoolSettings
  { _size    :: PoolSize
  , _timeout :: ConnTimeout
  } deriving (Generic, Eq, Show, FromDhall)
makeFieldsNoPrefix ''DbPoolSettings

data DbConfig = DbConfig { _hostName   :: HostName
                         , _dbPort     :: Port
                         , _user       :: DbUser
                         , _dbName     :: DbName
                         , _dbPassword :: DbPassword
                         } deriving (Generic, Eq, Show, FromDhall)
makeFieldsNoPrefix ''DbConfig

type ConnectionString = ByteString
