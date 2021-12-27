{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types.Logger where

import Control.Concurrent (Chan)
import Control.Lens.TH    (makeFieldsNoPrefix, makeLenses)
import TextShow           (TextShow(showb), fromText)
import TextShow.TH        (deriveTextShow)
import Universum          (Eq, Ord, Semigroup((<>)), Show, Text, id)

data Mode = NoLogging | Logging Level deriving (Eq , Show)

data Level = ERROR | WARN | DEBUG | INFO deriving (Eq, Ord, Show)
deriveTextShow ''Level

data Log = Log { _level  :: Level
               , _logMsg :: Text
               } deriving (Eq, Show)
makeLenses ''Log

instance TextShow Log where
  showb (Log lvl msg) = "[" <> showb lvl <> "] " <> fromText msg

newtype Logger = Logger (Chan Log)

newtype DummyLogger = DummyLogger { _logger :: Logger }
makeFieldsNoPrefix ''DummyLogger

instance HasLogger Logger Logger where
  logger = id
