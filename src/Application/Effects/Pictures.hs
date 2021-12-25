{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Application.Effects.Pictures ( module Application.Pictures
                                     ) where

import Universum

import Application
import Types.Environment
import Application.Pictures

import qualified Application.Pictures as Pictures

instance ( Monad m, HasPersistPicture env (Pictures.Handle m)
          ) => PersistPicture (AppM env m) where
  persist picture = do
    persistHandle <- view persistPicture
    lift $ view lpersist persistHandle picture
