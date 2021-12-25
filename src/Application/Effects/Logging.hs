{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Application.Effects.Logging ( module Application.Logging
                                    ) where

import Application
import Application.Logging
import Types.Environment
import Universum

import qualified Application.Logging as Logging

instance (HasLogging env (Logging.Handle m), Monad m
          ) => Logging (AppM env m) where
  log lvl txt = do
    logHandle <- view logging
    lift $ view llog logHandle lvl txt
