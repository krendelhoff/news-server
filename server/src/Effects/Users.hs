{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Effects.Users where

import Universum

import Types.Environment (AuthenticatedApp, HasUserId(getUserId))
import Types.Users       (ID(ID), Payload)

import Infrastructure    (Elem, run)
import Effects.CanReject (CanReject)
import Types.Auth        (Rights(Admin))

import qualified Database.Users as DB

class CanReject m => AcquireUser m where
  get :: m Payload

class AcquireUser m => ManageUser m where
  getById :: ID -> m (Maybe Payload)
  delete  :: ID -> m ()

instance (
          ) => AcquireUser (AuthenticatedApp rights) where
  get = getUserId >>= run . DB.getUnsafe

instance (Elem rights 'Admin ~ 'True
          ) => ManageUser (AuthenticatedApp rights) where
  getById = run . DB.get
  delete  = run . DB.delete
