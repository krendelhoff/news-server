{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
module Application.Effects.Utils where

import Control.Monad.Except (Monad, MonadTrans(lift))
import Universum


import Application       (HasUtils(utils))
import Application.Utils
import Crypto.Hash       (SHA256(SHA256), hashWith)
import Data.Time         (addUTCTime)
import Types.Auth
import Types.Environment
import Types.TH
import Types.Users
import Types.Utils

import qualified Application.Utils as Utils

class Monad m => UsesCurrentTime m where
  getCurrentTime :: m CurrentTime

getExpirationDate :: UsesCurrentTime m => m ExpirationDate
getExpirationDate = getCurrentTime <&> fromUTCTime . addUTCTime (5*60*60)
                                     . toUTCTime

class Monad m => GenRandom m where
  genRandomBytes :: Int -> m ByteString

generateTokens :: GenRandom m => m (AccessToken, RefreshToken)
generateTokens = ((,) <$> genRandomBytes 256 <*> genRandomBytes 256)
  <&> bimap bytesToAccessToken bytesToRefreshToken
  where bytesToAccessToken = fromText . fromString . show . hashWith SHA256
        bytesToRefreshToken = fromText . fromString . show . hashWith SHA256


instance (HasUtils env (Utils.Handle m), Monad m
          ) => UsesCurrentTime (AppM env m) where
  getCurrentTime = do
    utilsHandle <- view utils
    lift $ view lgetCurrentTime utilsHandle

instance (HasUtils env (Utils.Handle m), Monad m
          ) => GenRandom (AppM env m) where
  genRandomBytes n = do
    utilsHandle <- view utils
    lift $ view lgenRandomBytes utilsHandle n
