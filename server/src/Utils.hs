{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE LambdaCase #-}
module Utils where

import Control.Lens           (Iso', iso)
import Crypto.Hash            (hashWith)
import Crypto.Hash.Algorithms (Blake2b_256(Blake2b_256))
import Data.Aeson             (encode)
import Data.List              (lookup)
import Data.Time              (addUTCTime)
import Data.Time.Clock        (UTCTime)
import Data.Word8             (isSpace)
import Network.Wai            (Response, responseLBS)
import Universum              hiding (toText)

import qualified Crypto.Random   as Crypto
import qualified Data.ByteString as B
import qualified Data.Text       as T
import qualified Data.Time       as Time

import Infrastructure
import Types.Auth     (AccessToken)
import Types.Users    (Hash, Password)
import Types.Utils    (CurrentTime)

infixl 1 ?>>=
(?>>=) :: Monad m => m (Maybe a) -> (a -> m b) -> m (Maybe b)
m ?>>= f = m >>= \case
  Nothing -> return Nothing
  Just a  -> f a <&> Just

infixl 1 =>>
(=>>) :: Monad m => m a -> (a -> m b) -> m a
m =>> f = m >>= ((<$) <$> id <*> f)

parseToken :: ByteString -> Maybe AccessToken
parseToken (((fromText . T.strip <$>) . decodeUtf8' <$>)
             . B.break isSpace -> ("Bearer", Right token)) = Just token
parseToken _ = Nothing

getCurrentTime :: MonadIO m => m CurrentTime
getCurrentTime = liftIO Time.getCurrentTime <&> fromUTCTime

getRandomBytes :: MonadIO m => Int -> m ByteString
getRandomBytes = liftIO . Crypto.getRandomBytes

hash :: Password -> Hash
hash = fromText . fromString
     . show . hashWith Blake2b_256 . encodeUtf8 @Text @ByteString . toText
