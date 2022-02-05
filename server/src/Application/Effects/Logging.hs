{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Application.Effects.Logging where

import Universum

import Application         (HasLogging(..))
import Types.Environment   (AppM)
import Types.Logger        (Level)

import qualified Application.Logging as Logging

class Monad m => Logging m where
  log :: Level -> Text -> m ()

instance (HasLogging env (Logging.Handle m), Monad m
          ) => Logging (AppM env m) where
  log lvl txt = do
    logHandle <- view logging
    lift $ view Logging.log logHandle lvl txt
