{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
module Logger ( mkLog
              , Level(..)
              , Logger(..)
              , log
              , Log
              , HasLogger(..)
              , newLogger
              , level
              )where

import Control.Concurrent (Chan, newChan, writeChan)
import Control.Lens.TH
import TextShow           (TextShow, fromText, showb)
import TextShow.TH        (deriveTextShow)
import Universum          hiding (toText)

import Data.Coerce  (coerce)
import Types.Common hiding (fromText)

data Level = NONE | ERROR | WARN | DEBUG | INFO deriving (Eq, Ord, Show)
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

newtype Logger = Logger { _logger :: Chan Log }
makeFieldsNoPrefix ''Logger

log :: (HasLogger env Logger, MonadReader env m, MonadIO m) => Log -> m ()
log l = liftIO . flip writeChan l . coerce =<< view logger
