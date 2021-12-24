{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Application ( Handle
                   , HasUsers(..)
                   , new
                   )where

import Universum hiding (Handle)
import Control.Lens.TH
import Hasql.Pool

import Application.Users (Users)

import qualified Application.Users as Users

newtype Handle m = Handle { _users :: Users.Handle m }
makeFieldsNoPrefix ''Handle

new :: (MonadThrow m, MonadIO m) => Pool -> IO (Handle m)
new pl = do
  usersHandle <- Users.new pl
  return Handle { _users = usersHandle }

close :: (MonadThrow m, MonadIO m) => Handle m -> IO ()
close = const pass

withHandle :: (MonadMask m, MonadIO m) => Pool -> (Handle m -> IO a) -> IO a
withHandle pl = bracket (new pl) close
