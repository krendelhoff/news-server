{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Application.Logging where

import Universum hiding (Handle)
import Control.Lens.TH

import Logger ( Level, Logger )

import qualified Logger

class Monad m => Logging m where
  log :: Level -> Text -> m ()

newtype Handle m = Handle { _llog :: Level -> Text -> m () }
makeFieldsNoPrefix ''Handle

new :: MonadIO m => Logger -> IO (Handle m)
new lg = return $ Handle \lvl logStr -> runReaderT (Logger.log lvl logStr) lg

close :: Handle m -> IO ()
close = const pass

withHandle :: (MonadMask m, MonadIO m) => Logger -> (Handle m -> IO a) -> IO a
withHandle lg = bracket (new lg) close
