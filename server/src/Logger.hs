{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
module Logger ( Level(..)
              , Logger(..)
              , Log
              , HasLogger(..)
              , log
              , mkLog
              , newLogger
              , level
              , Mode(..)
              , mkLoggingThread
              )where

import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Data.Coerce                   (coerce)
import TextShow                      (showt)
import Universum                     hiding (toText)

import Types.Logger

mkLog :: Level -> Text -> Log
mkLog = Log

newLogger :: IO Logger
newLogger = coerce <$> newTQueueIO @Log

mkLoggingThread :: Mode -> Logger -> IO ()
mkLoggingThread (Logging upperLvl) logger = forever do
  msg <- atomically do readTQueue @Log (coerce logger)
  when (msg^.level <= upperLvl) do putStrLn . showt $ msg
mkLoggingThread NoLogging _ = pass

log :: ( HasLogger env Logger, MonadReader env m, MonadIO m
        ) => Level -> Text -> m ()
log lvl msg = liftIO . atomically . flip writeTQueue (mkLog lvl msg) . coerce =<< view logger
