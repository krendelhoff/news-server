{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Effects.Users where

import Universum hiding (Handle, get, fromString)

import Types.Users
import Types.TH
import Data.Time.Clock.POSIX
import Test.MonadStack
import Effects.Users

testId :: ID
testId = toID "14ebe699-397f-4ca9-bc76-120684167a1e"

instance AcquireUser (ReaderT (Handle PureMonad) PureMonad) where
  get = do
    handle <- ask
    lift $ tget handle

instance ManageUser (ReaderT (Handle PureMonad) PureMonad) where
  getById uid = do
    handle <- ask
    lift $ tgetById handle uid
  delete uid = do
    handle <- ask
    lift $ tdelete handle uid

creationTime :: CreationTime
creationTime = fromUTCTime $ posixSecondsToUTCTime 0

data Handle m = Handle
  { tget     :: m Payload
  , tgetById :: ID -> m (Maybe Payload)
  , tdelete  :: ID -> m ()
  }

payload :: Payload
payload = Payload testId "Savely" "Krendelhoff"
                  "krendelhoff" Nothing
                  creationTime (fromBool False)

adminPayload :: Payload
adminPayload = Payload testId "Savely" "Krendelhoff"
                       "krendelhoff" Nothing
                       creationTime (fromBool True)
