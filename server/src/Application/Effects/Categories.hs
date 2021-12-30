{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}
module Application.Effects.Categories where

import Universum

import Application
import Application.Categories   as Categories
import Application.Effects.Auth
import Infrastructure           hiding (user)
import Types.Categories
import Types.Environment

class AuthenticateUser m => AcquireCategory m where
  get          :: ID -> m (Maybe Payload)
  getRecursive :: ID -> m (Maybe PayloadRecursive)

class (AcquireCategory m, AuthenticateAdmin m) => PersistCategory m where
  create :: Title -> Maybe ID -> m (Maybe Payload)
  remove :: ID -> m NoContent
  rename :: ID -> Title -> m (Maybe Payload)
  rebase :: ID -> ID -> m (Maybe Payload)

instance (HasPersistCategories env (Categories.Handle m), Monad m
          , AuthenticateUser (AppM env m)
           ) => AcquireCategory (AppM env m) where
  get cid = do
    !uid <- user
    catHandle <- view persistCategories
    lift $ view lget catHandle cid
  getRecursive cid = do
    !uid <- user
    catHandle <- view persistCategories
    lift $ view lgetRecursive catHandle cid

instance (HasPersistCategories env (Categories.Handle m), AuthenticateAdmin (AppM env m)
          , Monad m, AcquireCategory (AppM env m)
           ) => PersistCategory (AppM env m) where
  create title mCid = do
    !uid <- whoami
    catHandle <- view persistCategories
    lift $ view lcreate catHandle title mCid
  remove cid = do
    !uid <- whoami
    catHandle <- view persistCategories
    lift $ view lremove catHandle cid
  rename cid title = do
    !uid <- whoami
    catHandle <- view persistCategories
    lift $ view lrename catHandle cid title
  rebase whatC whereC = do
    !uid <- whoami
    catHandle <- view persistCategories
    lift $ view lrebase catHandle whatC whereC
