{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
module Utils where

import Control.Lens    (Iso', iso)
import Crypto.Hash     (SHA256(SHA256), hashWith)
import Data.Aeson      (encode)
import Data.List       (lookup)
import Data.Time       (addUTCTime)
import Data.Time.Clock (UTCTime)
import Data.Word8      (isSpace)
import Network.Wai     (Response, responseLBS)
import Universum       hiding (toText)

import qualified Crypto.Random   as Crypto
import qualified Data.ByteString as B
import qualified Data.Text       as T
import qualified Data.Time       as Time

import Infrastructure
import Types.Auth     (AccessToken)
import Types.Users    (Hash, Password)
import Types.Utils    (CurrentTime)

parseToken :: ByteString -> Either TokenError AccessToken
parseToken (((fromText . T.strip <$>) . decodeUtf8' <$>)
             . B.break isSpace -> ("Bearer", Right token)) = Right token
parseToken _ = Left BadToken

getCurrentTime :: MonadIO m => m CurrentTime
getCurrentTime = liftIO Time.getCurrentTime <&> fromUTCTime

getRandomBytes :: MonadIO m => Int -> m ByteString
getRandomBytes = liftIO . Crypto.getRandomBytes

hash :: Password -> Hash
hash = fromText . fromString
     . show . hashWith SHA256 . encodeUtf8 @Text @ByteString . toText
