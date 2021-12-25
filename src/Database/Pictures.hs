{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Pictures where

import Hasql.TH
import Hasql.Transaction
import Universum

import Common
import Types.Pictures

import qualified Data.ByteString.Lazy as BL

upload :: BL.ByteString -> Transaction ID
upload (BL.toStrict -> picture) = fromUUID <$>
  statement picture [singletonStatement|
    INSERT INTO pictures (picture) VALUES ($1::bytea)
    RETURNING id::uuid
                    |]
