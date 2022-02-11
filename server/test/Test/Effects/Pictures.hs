{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Effects.Pictures where

import Universum hiding (get, Handle)
import Data.Coerce (coerce)

import Types.Pictures
import Effects.Pictures
import Test.MonadStack

import qualified Data.ByteString.Lazy as BL

testId :: ID
testId = toID "70f86a1e-dc11-4ec2-814e-ee75bd8defa6"

payload :: Payload
payload = coerce testId

instance PersistPicture (ReaderT (Handle PureMonad) PureMonad) where
  persist x = do
    handle <- ask
    lift $ tpersist handle x

data Handle m = Handle
  { tpersist :: BL.ByteString -> m ID }
