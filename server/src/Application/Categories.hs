{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Application.Categories where

import Control.Lens.TH
import Hasql.Pool      (Pool)
import Universum       hiding (Handle)

import Infrastructure
import Types.Categories

import qualified Database.Categories as DB

data Handle m = Handle { _lget :: ID -> m (Maybe Payload)
                       , _lgetRecursive :: ID -> m (Maybe PayloadRecursive)
                       , _lcreate :: Title -> Maybe ID -> m (Maybe Payload)
                       , _lremove :: ID -> m NoContent
                       , _lrename :: ID -> Title -> m (Maybe Payload)
                       , _lrebase :: ID -> ID -> m (Maybe Payload)
                       }
makeFieldsNoPrefix ''Handle

new :: (MonadIO m, MonadThrow m) => Pool -> IO (Handle m)
new pl = return $ Handle
  { _lget = \cid -> runReaderT (run (DB.get cid)) pl
  , _lgetRecursive = \cid -> runReaderT (run (DB.getRecursive cid)) pl
  , _lcreate = \t mI -> runReaderT (run (DB.create t mI)) pl
  , _lremove = \cid -> runReaderT (run (DB.remove cid)) pl
  , _lrename = \cid t -> runReaderT (run (DB.rename cid t)) pl
  , _lrebase = \cid pid -> runReaderT (run (DB.rebase cid pid)) pl
  }

close :: Handle m -> IO ()
close = const pass

withHandle :: (MonadMask m, MonadIO m) => Pool -> (Handle m -> IO a) -> IO a
withHandle pl = bracket (new pl) close
