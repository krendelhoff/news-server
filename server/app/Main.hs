{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import Control.Concurrent (forkIO)
import Dhall              (Text, input)
import Network.Wai        (Application)
import Universum          hiding (toText)

import qualified Hasql.Pool               as Pool
import qualified Network.Wai.Handler.Warp as Warp

--import API               (API, server)
import Infrastructure
import Logger
import Migration         (applyMigrations)
import Types.Environment

import qualified Application

serverSettings :: Warp.Settings
serverSettings = Warp.setBeforeMainLoop
                   (putStrLn @Text "Server initialized at localhost:3000...")
                   Warp.defaultSettings

--mkApp :: Environment Handler -> Application
--mkApp env = serve @API (unlift @API (runApp env) server)


main :: IO ()
main = do
  putStrLn @Text "boob"
 -- conf <- readFile "config.dhall" >>= input dbConfigDecoder
 -- logger <- newLogger
 -- forkIO do mkLoggingThread (Logging INFO) logger
 -- let poolSettings = (10, 5, mkConnStr conf)
 -- withPool poolSettings \pool -> do
 --   applyMigrations pool
 --   appHandle <- Application.new @Handler logger pool
 --   let env = Environment pool conf appHandle
 --   Warp.runSettings serverSettings (mkApp env)
