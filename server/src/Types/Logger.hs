{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types.Logger where

import Control.Concurrent.STM (TQueue)
import Control.Lens.TH        (makeFieldsNoPrefix, makeLenses)
import Data.Aeson             (FromJSON)
import Data.String            (IsString)
import TextShow               (TextShow(showb), fromText)
import TextShow.TH            (deriveTextShow)
import Universum

data Mode = NoLogging | Logging Level deriving (Eq , Show)

data Level = ERROR | WARN | INFO | DEBUG deriving (Eq, Ord, Show, Generic, FromJSON)
deriveTextShow ''Level

data Log = Log { _level  :: Level
               , _logMsg :: Text
               } deriving (Eq, Show)
makeLenses ''Log

instance TextShow Log where
  showb (Log lvl msg) = "[" <> showb lvl <> "] " <> fromText msg

newtype Logger = Logger (TQueue Log)

newtype DummyLogger = DummyLogger { _logger :: Logger }
makeFieldsNoPrefix ''DummyLogger

instance HasLogger Logger Logger where
  logger = id
