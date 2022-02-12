{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Effects.Categories where

import Universum hiding (get, Handle)

import Types.Categories hiding (title)
import Effects.Categories
import Test.MonadStack

testId :: ID
testId = toID "422a04c2-c62f-4192-9ac0-c4141e0d9e73"

title :: Title
title = "Set"

payload :: Payload
payload = Payload testId title (Just testParentId)

recursivePayload :: PayloadRecursive
recursivePayload = PayloadRecursive testId title
  (Just $ PayloadRecursive testParentId parentTitle Nothing)

testParentId :: ID
testParentId = toID "8a2aa26e-17fc-4b37-a8ef-60b50fa25306"

parentTitle :: Title
parentTitle = "Parent of the best category"

parentPayload :: Payload
parentPayload = Payload testParentId parentTitle Nothing

createForm :: CreateForm
createForm = CreateForm title (Just testParentId)

root :: ID
root = toID "3575928d-e11c-45c8-9dab-a68f90a05bfd"

data Handle m = Handle
  { tget          :: ID    -> m (Maybe Payload)
  , tgetRecursive :: ID    -> m (Maybe PayloadRecursive)
  , tcreate       :: Title -> Maybe ID -> m (Maybe Payload)
  , tremove       :: ID    -> m ()
  , trename       :: ID    -> Title -> m (Maybe Payload)
  , trebase       :: ID    -> ID -> m (Maybe Payload)
  , trootID       :: m ID
  }

instance AcquireCategory (ReaderT (Handle PureMonad) PureMonad) where
  get x = do
    handle <- ask
    lift $ tget handle x
  getRecursive x = do
    handle <- ask
    lift $ tgetRecursive handle x

instance PersistCategory (ReaderT (Handle PureMonad) PureMonad) where
  create x y = do
    handle <- ask
    lift $ tcreate handle x y
  remove x = do
    handle <- ask
    lift $ tremove handle x
  rename x y = do
    handle <- ask
    lift $ trename handle x y
  rebase x y = do
    handle <- ask
    lift $ trebase handle x y
  rootID = do
    handle <- ask
    lift $ trootID handle
