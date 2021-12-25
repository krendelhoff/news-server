{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Application.Effects.Users ( module Application.Users
                                  ) where

import Universum

import Application
import Application.Users
import Types.Environment
import Types.Router      (Handler)
import Types.Users       (HasUserId)

import qualified Application.Users as Users
import qualified Types.Users       as Users

instance ( Monad m, HasPersistUser env (Users.Handle m)
          , HasUserId env Users.ID
           ) => PersistUser env (AppM env m) where
  create name surname login mPicture pHash = do
    usersHandle <- view persistUser
    lift $ view lcreate usersHandle name surname login mPicture pHash
  get uid = do
    usersHandle <- view persistUser
    lift $ view lget usersHandle uid
