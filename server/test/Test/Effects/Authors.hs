{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Test.Effects.Authors where

import Universum hiding (get, Handle)
import Data.Coerce (coerce)

import Types.Authors
import Effects.Authors
import Test.MonadStack
import Effects.Users

import qualified Test.Effects.Users   as Users
import qualified Types.Users          as Users

testId :: ID
testId = toID "91a1a6d8-d7ad-4258-86c1-05c6ea2387ab"

desc :: Description
desc = "The best test author"

payload :: Payload
payload = Payload testId desc

updateForm :: UpdateForm
updateForm = coerce desc

data Handle m = Handle
  { tget       :: ID -> m (Maybe Payload)
  , tdowngrade :: ID -> m ()
  , tupdate    :: ID -> Description -> m (Maybe Payload)
  , tpromote   :: Users.ID -> Description -> m (Maybe Payload)
  , tusers     :: Users.Handle m
  }

instance AcquireUser (ReaderT (Handle PureMonad) PureMonad) where
  get = do
    handle <- asks tusers
    lift $ Users.tget handle

instance ManageUser (ReaderT (Handle PureMonad) PureMonad) where
  getById uid = do
    handle <- asks tusers
    lift $ Users.tgetById handle uid
  delete uid = do
    handle <- asks tusers
    lift $ Users.tdelete handle uid


instance AcquireAuthor (ReaderT (Handle PureMonad) PureMonad) where
  get x = do
    handle <- ask
    lift $ tget handle x

instance PersistAuthor (ReaderT (Handle PureMonad) PureMonad) where
  downgrade x = do
    handle <- ask
    lift $ tdowngrade handle x
  update x y = do
    handle <- ask
    lift $ tupdate handle x y
  promote x y = do
    handle <- ask
    lift $ tpromote handle x y
