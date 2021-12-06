{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DB where

import Hasql.Connection
import Universum

import Types.DB

withConn :: ConnectionString -> (Connection -> IO ()) -> IO ()
withConn connStr = bracket (new connStr) release
  where
    new = acquire >=> \case
      Left err   -> throwM err
      Right conn -> return conn
