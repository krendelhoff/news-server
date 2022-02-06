{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
module Types.Logger where

import Control.Concurrent.STM (TQueue)
import Control.Lens.TH        (makeFieldsNoPrefix, makeLenses)
import TextShow               (TextShow(showb), fromText)
import TextShow.TH            (deriveTextShow)
import Universum              (Eq, Ord, Semigroup((<>)), Show, Text, id)
import Data.String            (IsString)
import Dhall                  (Generic, FromDhall)

data Mode = NoLogging | Logging Level deriving (Eq , Show)

data Level = ERROR | WARN | DEBUG | INFO deriving (Eq, Ord, Show, Generic, FromDhall)
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
