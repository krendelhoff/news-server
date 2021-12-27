{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DB where

import Hasql.Connection           (Connection, acquire, release)
import Hasql.Pool                 (Pool)
import Hasql.Transaction          (Transaction)
import Hasql.Transaction.Sessions
    ( IsolationLevel(Serializable)
    , Mode(Write)
    , transaction
    )
import Universum

import qualified Hasql.Pool as Pool

import Types.DB
import Errors ()
import Types.Lenses (HasPool(..))


mkConnStr :: DbConfig -> ConnectionString
mkConnStr conf = encodeUtf8 $ "host=" <> conf^.hostName <> " "
                           <> "port=" <> conf^.port <> " "
                           <> "user=" <> conf^.user <> " "
                           <> "dbname=" <> conf^.dbname <> " "
                           <> "password=" <> conf^.dbPassword


withConn :: ConnectionString -> (Connection -> IO ()) -> IO ()
withConn connStr = bracket (new connStr) release
  where
    new = acquire >=> either throwM return


withPool :: Pool.Settings -> (Pool -> IO ()) -> IO ()
withPool s = bracket (Pool.acquire s) Pool.release


run :: ( HasPool env Pool, MonadThrow m, MonadReader env m, MonadIO m
        ) => Transaction a -> m a
run action = do
  view pool >>= liftIO . flip Pool.use (transaction Serializable Write action)
            >>= either throwM return