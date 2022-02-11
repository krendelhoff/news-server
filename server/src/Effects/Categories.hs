{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}
module Effects.Categories where

import Universum

import Infrastructure    hiding (user)
import Types.Categories
import Types.Environment

import Effects.CanReject (CanReject)
import Types.Auth

import qualified Database.Categories as DB

class (CanReject m, Monad m) => AcquireCategory m where
  get          :: ID -> m (Maybe Payload)
  getRecursive :: ID -> m (Maybe PayloadRecursive)

class AcquireCategory m => PersistCategory m where
  rootID :: m ID
  create :: Title -> Maybe ID -> m (Maybe Payload)
  remove :: ID -> m ()
  rename :: ID -> Title -> m (Maybe Payload)
  rebase :: ID -> ID -> m (Maybe Payload)

instance (
          ) => AcquireCategory (AuthenticatedApp rights) where
   get = run . DB.get
   getRecursive = run . DB.getRecursive

instance (Elem rights 'Admin ~ 'True
          ) => PersistCategory (AuthenticatedApp rights) where
  rootID = run DB.rootID
  create = (run .) . DB.create
  remove = run . DB.remove
  rename = (run .) . DB.rename
  rebase = (run .) . DB.rebase
