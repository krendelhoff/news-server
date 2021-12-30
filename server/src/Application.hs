{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
module Application ( Handle
                   , HasPersistUser(..)
                   , HasPersistPicture(..)
                   , HasLogging(..)
                   , HasAuth(..)
                   , HasUtils(..)
                   , HasPersistAuthor(..)
                   , new
                   ) where

import Control.Lens.TH (makeFieldsNoPrefix)
import Hasql.Pool      (Pool)
import Universum       hiding (Handle)

import Types.Logger (Logger)

import qualified Application.Auth     as Auth
import qualified Application.Logging  as Logging
import qualified Application.Pictures as Pictures
import qualified Application.Users    as Users
import qualified Application.Utils    as Utils
import qualified Application.Authors  as Authors



data Handle m = Handle { _persistUser    :: Users.Handle m
                       , _persistPicture :: Pictures.Handle m
                       , _logging        :: Logging.Handle m
                       , _auth           :: Auth.Handle m
                       , _utils          :: Utils.Handle m
                       , _persistAuthor  :: Authors.Handle m
                       }
makeFieldsNoPrefix ''Handle

new :: (MonadThrow m, MonadIO m) => Logger -> Pool -> IO (Handle m)
new logger pl = do
  _persistUser    <- Users.new pl
  _persistPicture <- Pictures.new pl
  _logging        <- Logging.new logger
  _auth           <- Auth.new pl
  _utils          <- Utils.new
  _persistAuthor  <- Authors.new pl
  return Handle{..}

close :: (MonadThrow m, MonadIO m) => Handle m -> IO ()
close = const pass

withHandle :: (MonadMask m, MonadIO m) => Logger -> Pool
                                       -> (Handle m -> IO a) -> IO a
withHandle lg pl = bracket (new lg pl) close
