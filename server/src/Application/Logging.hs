{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Application.Logging where

import Control.Lens.TH (makeFieldsNoPrefix)
import Logger          (Level, Logger)
import Universum       hiding (Handle)

import qualified Logger

newtype Handle m = Handle { _log :: Level -> Text -> m () }
makeFieldsNoPrefix ''Handle

new :: MonadIO m => Logger -> IO (Handle m)
new lg = return $ Handle \lvl logStr -> runReaderT (Logger.log lvl logStr) lg
