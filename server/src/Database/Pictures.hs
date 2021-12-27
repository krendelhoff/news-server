{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Pictures where

import Hasql.TH
import Hasql.Transaction (Transaction, statement)
import Universum

import Types.Pictures (ID)
import Infrastructure

import qualified Data.ByteString.Lazy as BL

upload :: BL.ByteString -> Transaction ID
upload (BL.toStrict -> picture) = fromUUID <$>
  statement picture [singletonStatement|
    INSERT INTO pictures (picture) VALUES ($1::bytea)
    RETURNING id::uuid
                    |]
