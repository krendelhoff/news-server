{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TypeApplications #-}
module Migration where

import Control.Exception
import Hasql.Connection
import Hasql.Migration
import Hasql.Session
import Hasql.Transaction.Sessions
import Paths_server               (getDataDir)
import Universum

import Types.DB

applyMigrations :: Connection -> IO ()
applyMigrations conn = do
  dataDir <- getDataDir
  let migrationsDir = dataDir <> "/migrations"
  loadMigrationsFromDirectory migrationsDir <&> (MigrationInitialization :)
    >>= traverse ( flip run conn
                 . transaction ReadCommitted Write
                 . runMigration ) >>= traverse_ \case
    Left queryError -> throwM queryError
    Right result -> maybe pass throwM result
