{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Application.Effects.Users where

import Universum

import Application              (HasPersistUser(..))
import Types.Environment        (AppM)
import Types.Router             (Handler)
import Types.Users              (Login, Name, Password, Payload, Surname)

import qualified Application.Users as Users
import qualified Types.Pictures    as Pictures
import qualified Types.Users       as Users

class Monad m => AcquireUser m where
  get    :: m Payload

class Monad m => PersistUser m where
  create :: Name -> Surname -> Login -> Maybe Pictures.ID
         -> Password -> m Users.ID

instance (Monad m, HasPersistUser env (Users.Handle m)
          ) => PersistUser (AppM env m) where
  create name surname login mPicture pHash = do
    usersHandle <- view persistUser
    lift $ view Users.create usersHandle name surname login mPicture pHash

instance (Monad m, HasPersistUser env (Users.Handle m)
          , MonadReader Users.ID m
           ) => AcquireUser (AppM env m) where
  get = do
    usersHandle <- view persistUser
    uid <- lift ask
    lift $ view Users.get usersHandle uid
