{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
module Application ( Handle
                   , HasPersistUser(..)
                   , HasPersistPicture(..)
                   , HasLogging(..)
                   , new
                   )where

import Control.Lens.TH
import Hasql.Pool
import Universum       hiding (Handle)

import Logger

import qualified Application.Pictures as Pictures
import qualified Application.Users    as Users
import qualified Application.Logging as Logging


data Handle m = Handle { _persistUser    :: Users.Handle m
                       , _persistPicture :: Pictures.Handle m
                       , _logging        :: Logging.Handle m
                       }
makeFieldsNoPrefix ''Handle

new :: (MonadThrow m, MonadIO m) => Logger -> Pool -> IO (Handle m)
new logger pl = do
  _persistUser <- Users.new pl
  _persistPicture <- Pictures.new pl
  _logging <- Logging.new logger
  return Handle{..}

close :: (MonadThrow m, MonadIO m) => Handle m -> IO ()
close = const pass

withHandle :: (MonadMask m, MonadIO m) => Logger -> Pool
                                       -> (Handle m -> IO a) -> IO a
withHandle lg pl = bracket (new lg pl) close
