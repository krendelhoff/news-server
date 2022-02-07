{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Effects.Users where

import Universum

import Types.Environment (AuthenticatedApp, HasUserId (getUserId))
import Types.Users       (Payload)

import Infrastructure (run)

import qualified Database.Users as DB

class Monad m => AcquireUser m where
  get :: m Payload

instance (
          ) => AcquireUser (AuthenticatedApp rights) where
  get = getUserId >>= run . DB.get
