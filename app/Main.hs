{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main where

import Control.Monad.Logger
import Data.Time                           (UTCTime)
import Data.UUID                           (UUID)
import Database.Esqueleto.Experimental     hiding (runMigration)
import Database.Persist.Migration          as Migration
    ( checkMigration
    , defaultSettings
    )
import Database.Persist.Migration.Postgres (runMigration)
import Database.Persist.Postgresql         (runSqlConn, withPostgresqlConn)
import Database.Persist.TH

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp  as Warp

import Universum


import API
import Migration  (migration)
import Types.UUID

mkPersist sqlSettings [persistLowerCase|
  User sql=users
    Id UUID sqltype=uuid default=uuid_generate_v4()
    name Text
    surname Text
    login Text
    avatar ByteString Maybe
    passwordHash Text
    createdAt UTCTime
    privileged Bool
    deriving Show Eq
|]

settings :: Settings
settings = setBeforeMainLoop (putStrLn @Text "Server started...") Warp.defaultSettings

connStr :: ByteString
connStr = "host=localhost port=5435 user=savely dbname=db password="

putUsers :: MonadIO m => SqlPersistT m ()
putUsers = do
  users <- select $ from $ table @User
  liftIO $ traverse_ (putStrLn . userName . entityVal) users

main :: IO ()
main = do
  runStderrLoggingT $
    withPostgresqlConn connStr \backend -> lift do
      runSqlConn (runMigration Migration.defaultSettings migration) backend
      runSqlConn putUsers backend
