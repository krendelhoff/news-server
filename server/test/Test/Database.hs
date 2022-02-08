{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Test.Database where

{-
import Hasql.Connection
import Hasql.Session              (QueryError, Session)
import Hasql.TH
import Hasql.Transaction
import Hasql.Transaction.Sessions
import Universum

import qualified Hasql.Session as HaSQL

import Types.DB

mainDbConnStr :: ConnectionString
mainDbConnStr = "host=localhost port=5435 dbname=db user=savely password="

testDbConnStr :: ConnectionString
testDbConnStr = "host=localhost port=5435 dbname=test user=savely password="

dropDatabase :: Session ()
dropDatabase = HaSQL.sql [uncheckedSql| DROP DATABASE test |]

createDatabase :: Session ()
createDatabase = HaSQL.sql [uncheckedSql| CREATE DATABASE test |]

applyMigration :: ByteString -> Transaction ()
applyMigration = sql

run :: Connection -> Transaction a -> IO (Either QueryError a)
run conn = flip HaSQL.run conn . transaction Serializable Write

setupDatabase :: IO ()
setupDatabase = do
  withConn mainDbConnStr \conn -> do
    flip HaSQL.run conn (dropDatabase >> createDatabase) >> pass
  withConn testDbConnStr \conn -> do
    run conn (
      applyMigration [uncheckedSqlFile|/home/savely/server/test/migrations/users.sql|])
      >> pass
  pass
-}
