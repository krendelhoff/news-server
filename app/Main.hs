{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import Control.Concurrent        (forkIO)
import Control.Concurrent.Chan   (readChan)
import Data.Aeson
import Data.Coerce               (coerce)
import Data.UUID                 (nil)
import Dhall
import Hasql.Connection          hiding (settings)
import Network.HTTP.Types.Status
import Network.Wai
import TextShow
import Universum                 hiding (toText)

import qualified Hasql.Pool               as Pool
import qualified Network.Wai.Handler.Warp as Warp

import API
import DB
import Logger
import Migration         (applyMigrations)
import Router
import Types.DB
import Types.Environment
import Types.Router
import Types.TH

import qualified Application

serverSettings :: Warp.Settings
serverSettings = Warp.setBeforeMainLoop
                   (putStrLn @Text "Server initialized at localhost:3000...")
                   Warp.defaultSettings

app :: Server API
app = server

main :: IO ()
main = do
  conf <- readFile "config.dhall" >>= input dbConfigDecoder
  logger <- newLogger
  forkIO do mkLoggingThread (Logging INFO) logger
  let poolSettings = (10, 5, mkConnStr conf)
  withPool poolSettings \pool -> do
    applyMigrations pool
    appHandle <- Application.new @Handler logger pool
    let ?env = Environment pool conf (fromUUID nil) appHandle logger
     in Warp.runSettings serverSettings $ serve @API app
