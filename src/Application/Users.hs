{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
module Application.Users where

import Control.Lens.TH
import Hasql.Pool
import Universum       hiding (Handle, get)

import DB           (run)
import Types.Common (HasPool(pool), Name)
import Types.Lenses (HasPool)
import Types.Users  hiding (login, surname)

import qualified Database.Users as DB
import qualified Types.Pictures as Pictures
import qualified Types.Users    as Users (ID)

type WithUserId env m = (MonadReader env m, HasUserId env Users.ID)

class (MonadReader env m, HasUserId env Users.ID, Monad m
       ) => PersistUser env m where
  create :: Name -> Surname -> Login -> Maybe Pictures.ID
         -> Hash -> m Users.ID
  get    :: Users.ID -> m Payload

data Handle m = Handle { _lcreate :: Name -> Surname -> Login
                                  -> Maybe Pictures.ID -> Hash -> m Users.ID
                       , _lget    :: Users.ID -> m Payload
                       }
makeFieldsNoPrefix ''Handle

new :: (MonadThrow m, MonadIO m) => Pool -> IO (Handle m)
new pl = return $ Handle
  { _lcreate = \name surname login mPicture passwordHash ->
      runReaderT (run (DB.create name surname login mPicture passwordHash)) pl
  , _lget = \uid -> runReaderT (run (DB.get uid)) pl
  }

close :: (MonadThrow m, MonadIO m) => Handle m -> IO ()
close = const pass

withHandle :: (MonadMask m, MonadIO m) => Pool -> (Handle m -> IO a) -> IO a
withHandle pl = bracket (new pl) close
