{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}
module Effects.Authors where

import Universum

import Effects.CanReject (CanReject)
import Infrastructure    (NoContent, run)
import Types.Auth        (Auth(Admin))
import Types.Authors     (Description, ID, Payload)
import Types.Environment (AuthenticatedApp)
import Utils             (Elem)

import qualified Database.Authors as DB

class (CanReject m, Monad m) => AcquireAuthor m where
  get :: ID -> m (Maybe Payload)

class AcquireAuthor m => PersistAuthor m where
  downgrade :: ID -> m NoContent
  update    :: ID -> Description -> m Payload

instance (Elem rights 'Admin ~ 'True
          ) => AcquireAuthor (AuthenticatedApp rights) where
  get = run . DB.get

instance (Elem rights 'Admin ~ 'True
          ) => PersistAuthor (AuthenticatedApp rights) where
  downgrade = run . DB.downgrade
  update = (run .) . DB.update
