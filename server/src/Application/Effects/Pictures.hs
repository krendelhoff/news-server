{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Application.Effects.Pictures where

import Universum

import Application       (HasPersistPicture(..))
import Types.Environment (AppM)
import Types.Pictures    (ID)

import qualified Application.Pictures as Pictures
import qualified Data.ByteString.Lazy as BL

class Monad m => PersistPicture m where
  persist :: BL.ByteString -> m ID

instance ( Monad m, HasPersistPicture env (Pictures.Handle m)
          ) => PersistPicture (AppM env m) where
  persist picture = do
    persistHandle <- view persistPicture
    lift $ view Pictures.persist persistHandle picture
