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
import Infrastructure             (failGracefully)
import Paths_server               (getDataDir)
import Universum                  hiding (use)

applyMigrations :: Pool -> IO ()
applyMigrations pool = do
  dataDir <- getDataDir
  let migrationsDir = dataDir <> "/migrations"
  loadMigrationsFromDirectory migrationsDir <&> (MigrationInitialization :)
    >>= traverse ( use pool
                 . transaction Serializable Write
                 . runMigration ) >>= traverse_ \case
    Left migrationError -> failGracefully (SomeException migrationError)
    Right result        -> maybe pass (failGracefully . SomeException) result
