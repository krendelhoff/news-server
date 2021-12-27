{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Migration where

import Hasql.Migration
    ( MigrationCommand(MigrationInitialization)
    , loadMigrationsFromDirectory
    , runMigration
    )
import Hasql.Pool                 (Pool, use)
import Hasql.Transaction.Sessions
    ( IsolationLevel(Serializable)
    , Mode(Write)
    , transaction
    )
import Paths_server               (getDataDir)
import Universum                  hiding (use)

import Infrastructure

applyMigrations :: Pool -> IO ()
applyMigrations pool = do
  dataDir <- getDataDir
  let migrationsDir = dataDir <> "/migrations"
  loadMigrationsFromDirectory migrationsDir <&> (MigrationInitialization :)
    >>= traverse ( use pool
                 . transaction Serializable Write
                 . runMigration ) >>= traverse_ \case
    Left usageError -> throwM usageError
    Right result    -> maybe pass throwM result
