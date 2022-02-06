{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import Control.Concurrent (forkIO)
import Dhall              (Text, input, auto)
import Network.Wai        (Application)
import Universum          hiding (toText)

import qualified Hasql.Pool               as Pool
import qualified Network.Wai.Handler.Warp as Warp

import Server            (API, server)
import Server.Auth (authenticate)
import Utils (parseToken)
import Infrastructure
import Logger
import Migration         (applyMigrations)
import Types.Environment
import Types.Auth

import qualified Application

warpSettings :: Warp.Settings
warpSettings = Warp.setBeforeMainLoop
                 (putStrLn @Text "Server initialized at localhost:3000...")
                 Warp.defaultSettings

mkApp :: ServingHandle -> Environment Handler -> Application
mkApp handle env = serve @API handle (unlift @API (runApp env) server)

main :: IO ()
main = do
  -- TODO add optparse-applicative
  conf <- readFile "config.dhall" >>= input auto
  print @Config conf
  undefined{-
  logger <- newLogger
  forkIO do mkLoggingThread (Logging INFO) logger
  let poolSettings = (10, 5, undefined) --mkConnStr conf)
  withPool poolSettings \pool -> do
    applyMigrations pool
    appHandle <- Application.new @Handler logger pool
    let env = Environment pool conf appHandle
        servingHandle = ServingHandle
          { _extractToken = either (const Nothing) (Just . toText) . parseToken
          , _authenticate = flip runReaderT pool . authenticate
          , _log          = flip runReaderT logger . log ERROR
          }
    Warp.runSettings warpSettings (mkApp servingHandle env)
-}
