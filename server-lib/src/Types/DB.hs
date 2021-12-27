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

import Control.Lens.TH (makeClassy)
import Dhall           (Decoder, Generic, Text, field, record, strictText)
import Universum



-- TODO TH CREATION
newtype HostName = HostName Text deriving stock (Eq, Show)
                                 deriving newtype (IsString)

newtype Port = Port Word16 deriving stock (Eq, Show, Ord)
                           deriving newtype (Num)

data DbConfig = DbConfig { _hostName   :: Text
                         , _port       :: Text
                         , _user       :: Text
                         , _dbname     :: Text
                         , _dbPassword :: Text
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
