{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Effects.Pictures where

import Universum

import Infrastructure    (run)
import Types.Environment (AuthenticatedApp)
import Types.Pictures    (ID)

import qualified Data.ByteString.Lazy as BL
import qualified Database.Pictures    as DB

class Monad m => PersistPicture m where
  persist :: BL.ByteString -> m ID

instance (
          ) => PersistPicture (AuthenticatedApp rights) where
  persist = run . DB.upload
