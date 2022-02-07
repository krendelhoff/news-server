{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import Control.Concurrent (forkIO)
import Data.Yaml          (ParseException, decodeFileThrow)
import Hasql.Pool         (Pool)
import Network.Wai        (Application)
import Universum          hiding (toText)

import qualified Hasql.Pool               as Pool
import qualified Network.Wai.Handler.Warp as Warp

import Infrastructure
import Logger
import Migration (applyMigrations)
import Server    (API, server)
import Server.Auth       (authenticate)
import Types.Environment
import Utils             (parseToken)

main :: IO ()
main = do
  -- TODO add optparse-applicative
  conf <- readConfigurationFile "config.yaml"
  logger <- newLogger
  forkIO do mkLoggingThread (Logging INFO) logger
  withPool (conf^.dbConfig) (conf^.dbPoolSettings) \pool -> do
    applyMigrations pool
    let env = Environment pool conf logger
        warpSettings = mkWarpSettings (conf^.port)
        servingHandle = mkServingHandle pool logger
    Warp.runSettings warpSettings (mkApp servingHandle env)

readConfigurationFile :: FilePath -> IO Config
readConfigurationFile configFilePath =
  decodeFileThrow @IO @Config configFilePath `catch` \(e :: ParseException) -> do
    putStrLn @Text "Configuration file format violation"
    throwM e

mkWarpSettings :: Port -> Warp.Settings
mkWarpSettings portNumber =
  Warp.setBeforeMainLoop (putStrLn @Text message) Warp.defaultSettings
  where
    message = "Server initialized at localhost:" <> show portNumber

mkServingHandle :: Pool -> Logger -> ServingHandle
mkServingHandle pool logger = ServingHandle
  { _extractToken = either (const Nothing) (Just . toText) . parseToken
  , _authenticate = flip runReaderT pool . authenticate
  , _log          = flip runReaderT logger . log ERROR
  }

mkApp :: ServingHandle -> Environment -> Application
mkApp handle env = serve @API handle (unlift @API (runApp env) server)
