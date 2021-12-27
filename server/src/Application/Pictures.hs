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

newtype Handle m = Handle { _lpersist :: BL.ByteString -> m ID }
makeFieldsNoPrefix ''Handle

new :: (MonadThrow m, MonadIO m) => Pool -> IO (Handle m)
new pl = return $ Handle
  { _lpersist = \picture -> runReaderT (run (DB.upload picture)) pl }

close :: Handle m -> IO ()
close = const pass

withHandle :: (MonadMask m, MonadIO m) => Pool -> (Handle m -> IO a) -> IO a
withHandle pl = bracket (new pl) close
