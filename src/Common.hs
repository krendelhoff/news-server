{-# LANGUAGE TypeApplications #-}
module Common ( module Logger
              , getCurrentTime
              , getExpirationDate
              , utctime
              , hash
              , generateToken
              ) where

import Control.Lens    (Iso', iso)
import Crypto.Hash     hiding (hash)
import Data.Time       (addUTCTime)
import Data.Time.Clock (UTCTime)
import Universum       hiding (toText)

import qualified Crypto.Random as Crypto
import qualified Data.Time     as Time

import Logger
import Types.Common
import Types.TH
import Types.Users


getCurrentTime :: MonadIO m => m CurrentTime
getCurrentTime = liftIO Time.getCurrentTime <&> fromUTCTime

getRandomBytes :: MonadIO m => Int -> m ByteString
getRandomBytes = liftIO . Crypto.getRandomBytes

generateToken :: MonadIO m => m Token
generateToken = getRandomBytes 256 <&> fromText . fromString . show
                                     . hashWith SHA256

hash :: Password -> Hash
hash = fromText . fromString . show
     . hashWith SHA256 . encodeUtf8 @Text @ByteString . toText

getExpirationDate ::  MonadIO m => m ExpirationDate
getExpirationDate = getCurrentTime <&> fromUTCTime
                                     . addUTCTime (5*60*60)
                                     . toUTCTime


utctime :: IsUTCTime a => Iso' a UTCTime
utctime = iso toUTCTime fromUTCTime
