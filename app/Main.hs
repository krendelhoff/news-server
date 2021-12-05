{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Main where

import Data.Text.Read
import Database.Persist.Migration          as Migration
    ( checkMigration
    , defaultSettings
    )
import Database.Persist.Migration.Postgres (runMigration)
import Database.Persist.Postgresql         (withPostgresqlConn, runSqlConn, withPostgresqlPool)
import Network.HTTP.Types.Status
import Network.Wai
import Control.Monad.Logger
import Network.Wai.Handler.Warp            as Warp
import Universum

import qualified Data.ByteString.Lazy.Char8 as BLC

import API
import Migration (migration)
import Database.Persist.Sql (runSqlPersistMPool)


app :: Application
app req respond = respond $ responseLBS status200 [] (BLC.pack . show $ req)

type MyAPI = "boba" :> "biba" :> Get Int

betterApp :: Server MyAPI
betterApp = return 5

settings :: Settings
settings = setBeforeMainLoop (putStrLn @Text "Server started...") Warp.defaultSettings

connStr :: ByteString
connStr = "host=localhost port=5435 user=savely dbname=db password="

main :: IO ()
main = do
  runStderrLoggingT $
    withPostgresqlConn connStr \backend -> do
      lift $
        runSqlConn (runMigration Migration.defaultSettings migration) backend
