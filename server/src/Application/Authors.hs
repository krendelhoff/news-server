{-# LANGUAGE TemplateHaskell #-}

module Application.Authors where

import Universum hiding (Handle)
import Hasql.Pool (Pool)
import Control.Lens.TH

import Types.Authors
import Infrastructure

import qualified Database.Authors as DB

data Handle m = Handle { _downgrade :: ID -> m NoContent
                       , _update    :: ID -> Description -> m Payload
                       , _get       :: ID -> m (Maybe Payload)
                       }
makeLenses ''Handle

new :: (MonadThrow m, MonadIO m) => Pool -> IO (Handle m)
new pl = return $
  Handle { _downgrade = \aid -> runReaderT (run (DB.downgrade aid)) pl
         , _update = \aid desc -> runReaderT (run (DB.update aid desc)) pl
         , _get = \aid -> runReaderT (run (DB.get aid)) pl
         }
