{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Effects.Utils where

import Control.Monad.Except (Monad, MonadTrans(lift))
import Universum
import Crypto.Hash       (SHA256(SHA256), hashWith)
import Data.Time         (addUTCTime)

import Infrastructure
import Types.Auth
import Types.Environment
import Types.Users
import Types.Utils

import qualified Utils

class Monad m => UsesCurrentTime m where
  getCurrentTime :: m CurrentTime

getExpirationDate :: UsesCurrentTime m => m ExpirationDate
getExpirationDate = getCurrentTime <&> fromUTCTime . addUTCTime (5*60*60*60)
                                     . toUTCTime

class Monad m => GenRandom m where
  genRandomBytes :: Int -> m ByteString

generateTokens :: GenRandom m => m (AccessToken, RefreshToken)
generateTokens = ((,) <$> genRandomBytes 256 <*> genRandomBytes 256)
  <&> bimap bytesToAccessToken bytesToRefreshToken
  where bytesToAccessToken = fromText . fromString . show . hashWith SHA256
        bytesToRefreshToken = fromText . fromString . show . hashWith SHA256


instance (
          ) => UsesCurrentTime (AuthenticatedApp rights) where
  getCurrentTime = Utils.getCurrentTime

instance (
          ) => UsesCurrentTime App where
  getCurrentTime = Utils.getCurrentTime

instance (
          ) => GenRandom (AuthenticatedApp rights) where
  genRandomBytes = Utils.getRandomBytes

instance (
          ) => GenRandom App where
  genRandomBytes = Utils.getRandomBytes
