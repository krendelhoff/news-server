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

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Lens.TH
import TextShow           (TextShow(showt), fromText, showb)
import TextShow.TH        (deriveTextShow)
import Universum          hiding (toText)

import Data.Coerce  (coerce)

import Types.Lenses

data Mode = NoLogging | Logging Level deriving (Eq , Show)

data Level = ERROR | WARN | DEBUG | INFO deriving (Eq, Ord, Show)
deriveTextShow ''Level

data Log = Log { _level  :: Level
               , _logMsg :: Text
               } deriving (Eq, Show)
makeLenses ''Log

instance TextShow Log where
  showb (Log lvl msg) = "[" <> showb lvl <> "] " <> fromText msg

mkLog :: Level -> Text -> Log
mkLog = Log

newLogger :: IO Logger
newLogger = coerce <$> newChan @Log

newtype Logger = Logger (Chan Log)

mkLoggingThread :: Mode -> Logger -> IO ()
mkLoggingThread (Logging upperLvl) logger = forever do
  msg <- readChan @Log (coerce logger)
  when (msg^.level <= upperLvl) do putStrLn @Text . showt $ msg
mkLoggingThread NoLogging _ = pass

newtype DummyLogger = DummyLogger { _logger :: Logger }
makeFieldsNoPrefix ''DummyLogger

instance HasLogger Logger Logger where
  logger = id

log :: ( HasLogger env Logger, MonadReader env m, MonadIO m
        ) => Level -> Text -> m ()
log lvl msg = liftIO . flip writeChan (mkLog lvl msg) . coerce =<< view logger
