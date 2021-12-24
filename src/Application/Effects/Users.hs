{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Application.Effects.Users ( module Application.Users
                                  ) where

import Universum

import Application
import Application.Users
import Types.Environment
import Types.Users (HasUserId)
import Types.Router      (Handler)

import qualified Types.Users       as Users

instance ( Monad m, HasApplication env (Application.Handle m)
          , HasUserId env Users.ID
           ) => Users (ReaderT env m) where
  create name surname login mPicture pHash = do
    appHandle <- view application
    lift $ view (users . lcreate) appHandle name surname login mPicture pHash
  get uid = do
    appHandle <- view application
    lift $ view (users . lget) appHandle uid
