{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}
module Application.Effects.Categories where

import Universum

import Application
import Application.Effects.Auth
import Infrastructure           hiding (user)
import Types.Categories
import Types.Environment

import qualified Application.Categories   as Categories

class Monad m => AcquireCategory m where
  get          :: ID -> m (Maybe Payload)
  getRecursive :: ID -> m (Maybe PayloadRecursive)

class AcquireCategory m => PersistCategory m where
  create :: Title -> Maybe ID -> m (Maybe Payload)
  remove :: ID -> m NoContent
  rename :: ID -> Title -> m (Maybe Payload)
  rebase :: ID -> ID -> m (Maybe Payload)

instance (HasPersistCategories env (Categories.Handle m), Monad m
          ) => AcquireCategory (AppM env m) where
  get cid = do
    catHandle <- view persistCategories
    lift $ view Categories.get catHandle cid
  getRecursive cid = do
    catHandle <- view persistCategories
    lift $ view Categories.getRecursive catHandle cid

instance (HasPersistCategories env (Categories.Handle m)
          , Monad m, AcquireCategory (AppM env m)
           ) => PersistCategory (AppM env m) where
  create title mCid = do
    catHandle <- view persistCategories
    lift $ view Categories.create catHandle title mCid
  remove cid = do
    catHandle <- view persistCategories
    lift $ view Categories.remove catHandle cid
  rename cid title = do
    catHandle <- view persistCategories
    lift $ view Categories.rename catHandle cid title
  rebase whatC whereC = do
    catHandle <- view persistCategories
    lift $ view Categories.rebase catHandle whatC whereC
