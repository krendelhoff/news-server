{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}
module DB ( Transaction
          , singletonStatement
          , vectorStatement
          , maybeStatement
          , rowsAffectedStatement
          , resultlessStatement
          , statement
          , mkConnStr
          , run
          , withPool
          , withConn
          ) where

import Hasql.Connection           (Connection, acquire, release)
import Hasql.Pool                 (Pool)
import TextShow                   (showt)
import Errors                     ()
import Hasql.Connection           (ConnectionError)
import Hasql.TH
import Hasql.Pool                 (UsageError)
import Hasql.Transaction          (Transaction, statement)
import Hasql.Transaction.Sessions
    ( IsolationLevel(Serializable)
    , Mode(Write)
    , transaction
    )
import Universum hiding (toText)

import qualified Hasql.Pool as Pool
import qualified Data.Text as T

import Types.DB
import Types.TH
import Types.Lenses (HasPool(..))

mkConnStr :: DbConfig -> ConnectionString
mkConnStr conf = encodeUtf8 $
  T.intercalate " " [ "host=" <> toText (conf^.hostName)
                    , "port=" <> showt (conf^.port)
                    , "user=" <> toText (conf^.user)
                    , "dbname=" <> toText (conf^.dbName)
                    , "password=" <> toText (conf^.dbPassword)
                    ]

withConn :: ConnectionString -> (Connection -> IO ()) -> IO ()
withConn connStr = bracket (new connStr) release
  where
    new = acquire >=> either rethrowFunc return
    rethrowFunc :: ConnectionError -> IO a
    rethrowFunc e = do
      putStrLn @Text "Can't acquire resources for database connection!"
      throwM e

withPool :: DbConfig -> DbPoolSettings -> (Pool -> IO ()) -> IO ()
withPool (mkConnStr -> connStr) (DbPoolSettings size timeout) =
  (`catch` rethrowFunc) . bracket (Pool.acquire settings) Pool.release
  where
    settings =
      (fromIntegral . toInt $ size, fromIntegral . toInt $ timeout, connStr)
    rethrowFunc :: UsageError -> IO a
    rethrowFunc e = do
      putStrLn @Text "Can't acquire database connection pool!"
      throwM e

run :: (HasPool env Pool, MonadThrow m, MonadReader env m, MonadIO m
        ) => Transaction a -> m a
run action = do
  view pool >>= liftIO . flip Pool.use (transaction Serializable Write action)
            >>= either throwM return
