{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Application.Pictures where

import Control.Lens.TH (makeFieldsNoPrefix)
import Hasql.Pool      (Pool)
import Universum       hiding (Handle)

import qualified Data.ByteString.Lazy as BL

import Types.Pictures ( ID )
import Infrastructure

import qualified Database.Pictures as DB

newtype Handle m = Handle { _persist :: BL.ByteString -> m ID }
makeFieldsNoPrefix ''Handle

new :: (MonadThrow m, MonadIO m) => Pool -> IO (Handle m)
new pl = return $ Handle
  { _persist = \picture -> runReaderT (run (DB.upload picture)) pl }
