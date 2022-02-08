{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Types.DB where

import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Aeson      (FromJSON)
import TextShow.TH
import Deriving.Aeson
import Universum

import Data.Time (NominalDiffTime)
import Types.TH
import TextShow (TextShow)

newIntType  "PoolSize"
newIntType  "ConnTimeout"
newTextType "HostName"
newTextType "DbUser"
newTextType "DbName"
newTextType "DbPassword"

newtype Port = Port Word16 deriving stock (Eq, Generic)
                           deriving newtype (Show, TextShow, Ord, Num, FromJSON)

data DbPoolSettings = DbPoolSettings
  { _size    :: PoolSize
  , _timeout :: ConnTimeout
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON)
    via (CustomJSON '[FieldLabelModifier '[StripPrefix "_"]] DbPoolSettings)
makeFieldsNoPrefix ''DbPoolSettings

data DbConfig = DbConfig
  { _hostName   :: HostName
  , _port       :: Port
  , _user       :: DbUser
  , _dbName     :: DbName
  , _dbPassword :: DbPassword
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON)
    via (CustomJSON '[FieldLabelModifier '[StripPrefix "_"]] DbConfig)
makeFieldsNoPrefix ''DbConfig

type ConnectionString = ByteString
