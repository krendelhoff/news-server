{-# LANGUAGE TemplateHaskell #-}

module Application.Authors where

import Universum hiding (Handle)
import Hasql.Pool (Pool)
import Control.Lens.TH

import Types.Authors
import Infrastructure

import qualified Database.Authors as DB

data Handle m = Handle { _ldowngrade :: ID -> m NoContent
                       , _lupdate    :: ID -> Description -> m Payload
                       , _lget       :: ID -> m (Maybe Payload)
                       }
makeLenses ''Handle

new :: (MonadThrow m, MonadIO m) => Pool -> IO (Handle m)
new pl = return $
  Handle { _ldowngrade = \aid -> runReaderT (run (DB.downgrade aid)) pl
         , _lupdate = \aid desc -> runReaderT (run (DB.update aid desc)) pl
         , _lget = \aid -> runReaderT (run (DB.get aid)) pl
         }

close :: Handle m -> IO ()
close = const pass

withHandle :: (MonadMask m, MonadIO m) => Pool -> (Handle m -> IO a) -> IO a
withHandle pl = bracket (new pl) close
