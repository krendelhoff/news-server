{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
module Effects.Authors where

import Universum hiding (get)

import Effects.CanReject (CanReject)
import Effects.Users     (ManageUser (getById))
import Infrastructure    (Elem, NoContent, run)
import Types.Auth        (Rights(Admin))
import Types.Authors     (Description, ID, Payload (Payload))
import Utils ((?>>=))
import Types.Environment (AuthenticatedApp)

import qualified Database.Authors as DB
import qualified Types.Users as Users
import Types.Users (userId)

class ManageUser m => AcquireAuthor m where
  get :: ID -> m (Maybe Payload)

class AcquireAuthor m => PersistAuthor m where
  downgrade :: ID -> m ()
  update    :: ID -> Description -> m (Maybe Payload)
  promote   :: Users.ID -> Description -> m (Maybe Payload)

instance (Elem rights 'Admin ~ 'True
          ) => AcquireAuthor (AuthenticatedApp rights) where
  get = run . DB.get

instance (Elem rights 'Admin ~ 'True
          ) => PersistAuthor (AuthenticatedApp rights) where
  downgrade = run . DB.downgrade
  update mAid desc =
    get mAid ?>>= run . flip DB.updateUnsafe desc . view userId
  promote mUid desc =
    getById mUid ?>>= run . flip DB.promoteUnsafe desc . view userId
