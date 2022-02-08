{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Utils where

import Control.Lens    (Iso', iso)
import Crypto.Hash     (hashWith)
import Crypto.Hash.Algorithms (Blake2b_256(Blake2b_256))
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

type family Elem (lst :: [a]) (el :: a) where
  Elem '[] a       = 'False
  Elem (a ': xs) a = 'True
  Elem (x ': xs) a = Elem xs a

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
