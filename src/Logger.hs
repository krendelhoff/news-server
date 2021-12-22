{-# LANGUAGE AllowAmbiguousTypes        #-}
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
{-# LANGUAGE BlockArguments #-}
module Logger ( Level(..)
              , Logger(..)
              , log
              , Log
              , HasLogger(..)
              , newLogger
              , level
              , Mode(..)
              , mkLoggingThread
              )where

import Control.Concurrent (Chan, newChan, writeChan, readChan)
import Control.Lens.TH
import TextShow           (TextShow (showt), fromText, showb)
import TextShow.TH        (deriveTextShow)
import Universum          hiding (toText)

import Data.Coerce  (coerce)
import Types.Common hiding (fromText)

data Mode = NoLogging | Logging Level deriving (Eq , Show)

data Level = ERROR | WARN | DEBUG | INFO deriving (Eq, Ord, Show)
deriveTextShow ''Level
makeKnown ''Level

data Log = Log { _level  :: Level
               , _logMsg :: Text
               } deriving (Eq, Show)
makeLenses ''Log

instance TextShow Log where
  showb (Log lvl msg) = "[" <> showb lvl <> "] " <> fromText msg

mkLog :: forall lvl. KnownLevel lvl => Text -> Log
mkLog = Log (levelVal @lvl)

newLogger :: IO Logger
newLogger = coerce <$> newChan @Log

newtype Logger = Logger { _logger :: Chan Log }
makeFieldsNoPrefix ''Logger

mkLoggingThread :: Mode -> Logger -> IO ()
mkLoggingThread (Logging upperLvl) logger = forever do
  msg <- readChan @Log (coerce logger)
  when (msg^.level <= upperLvl) do putStrLn @Text . showt $ msg
mkLoggingThread NoLogging _ = pass

log :: forall lvl env m. ( HasLogger env Logger, MonadReader env m, MonadIO m
                         , KnownLevel lvl ) => Text -> m ()
log msg = liftIO . flip writeChan (mkLog @lvl msg) . coerce =<< view logger
