{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DB where

import Hasql.Connection
import Hasql.Transaction
import Hasql.Transaction.Sessions
import Universum
import Hasql.Pool (Pool)

import qualified Hasql.Pool as Pool

import Types.DB

withConn :: ConnectionString -> (Connection -> IO ()) -> IO ()
withConn connStr = bracket (new connStr) release
  where
    new = acquire >=> \case
      Left err   -> throwM err
      Right conn -> return conn

withPool :: Pool.Settings -> (Pool -> IO ()) -> IO ()
withPool s = bracket (Pool.acquire s) Pool.release


run :: (HasPool env Pool, MonadThrow m, MonadReader env m, MonadIO m) =>
       Transaction a -> m a
run action = do
  connectionPool <- view pool
  liftIO (Pool.use connectionPool (transaction Serializable Write action))
    >>= \case
      Left err -> throwM err
      Right a -> return a
