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

data Handle m = Handle { _get :: ID -> m (Maybe Payload)
                       , _getRecursive :: ID -> m (Maybe PayloadRecursive)
                       , _create :: Title -> Maybe ID -> m (Maybe Payload)
                       , _remove :: ID -> m NoContent
                       , _rename :: ID -> Title -> m (Maybe Payload)
                       , _rebase :: ID -> ID -> m (Maybe Payload)
                       }
makeFieldsNoPrefix ''Handle

new :: (MonadIO m, MonadThrow m) => Pool -> IO (Handle m)
new pl = return $ Handle
  { _get = \cid -> runReaderT (run (DB.get cid)) pl
  , _getRecursive = \cid -> runReaderT (run (DB.getRecursive cid)) pl
  , _create = \t mI -> runReaderT (run (DB.create t mI)) pl
  , _remove = \cid -> runReaderT (run (DB.remove cid)) pl
  , _rename = \cid t -> runReaderT (run (DB.rename cid t)) pl
  , _rebase = \cid pid -> runReaderT (run (DB.rebase cid pid)) pl
  }
