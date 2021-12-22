{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
module Common ( module Logger
              , module Errors
              , module Types.Common
              , module DB
              , getCurrentTime
              , getExpirationDate
              , utctime
              , hash
              , generateToken
              , toResponse
              , extractToken
              ) where

import Control.Lens       (Iso', iso)
import Crypto.Hash        hiding (hash)
import Data.Aeson         (encode)
import Data.List          (lookup)
import Data.Time          (addUTCTime)
import Data.Time.Clock    (UTCTime)
import Data.Word8         (isSpace)
import Network.HTTP.Types
import Network.Wai        (Response, responseLBS)
import Universum          hiding (toText)

import qualified Crypto.Random   as Crypto
import qualified Data.ByteString as B
import qualified Data.Text       as T
import qualified Data.Time       as Time

import Logger
import Errors
import Types.Common
import DB
import Types.Users


-- TODO normal module structure
toResponse :: ServerError -> Response
toResponse (ServerError st m) = responseLBS st [] (encode m)

extractToken :: RequestHeaders -> Either TokenError Token
extractToken hMap = case lookup "Authorization" hMap of
  Nothing -> Left NoToken
  Just (((fromText . T.strip <$>) . decodeUtf8' <$>)
       . B.break isSpace -> ("Bearer", Right token)) -> Right token
  _ -> Left BadToken

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
