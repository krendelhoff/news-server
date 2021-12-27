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

import Control.Concurrent (newChan, readChan, writeChan)
import Universum hiding (toText)
import Data.Coerce (coerce)

import Types.Logger

mkLog :: Level -> Text -> Log
mkLog = Log

newLogger :: IO Logger
newLogger = coerce <$> newChan @Log

mkLoggingThread :: Mode -> Logger -> IO ()
mkLoggingThread (Logging upperLvl) logger = forever do
  msg <- readChan @Log (coerce logger)
  when (msg^.level <= upperLvl) do putStrLn @Text . show $ msg
mkLoggingThread NoLogging _ = pass

log :: ( HasLogger env Logger, MonadReader env m, MonadIO m
        ) => Level -> Text -> m ()
log lvl msg = liftIO . flip writeChan (mkLog lvl msg) . coerce =<< view logger
