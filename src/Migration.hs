{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Migration where

import Control.Exception
import Hasql.Migration
import Hasql.Pool
import Hasql.Transaction
import Hasql.Transaction.Sessions
import Paths_server               (getDataDir)
import Universum                  hiding (use)

import Types.DB
import Common

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
