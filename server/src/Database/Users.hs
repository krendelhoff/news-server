{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Users where

import Hasql.TH          (singletonStatement)
import Hasql.Transaction (Transaction, statement)
import Universum         hiding (toText)

import Types.Users
import Infrastructure
import Utils       (hash)

import qualified Types.Pictures as Pictures
import qualified Types.Users    as Users (ID)

get :: Users.ID -> Transaction Payload
get (toUUID -> uid) = encodePayload <$> statement uid
  [singletonStatement|
     SELECT id::uuid, name::text
          , surname::text, login::text
          , avatar::uuid?, created_at::timestamptz
          , privileged::bool
     FROM users
     WHERE id = $1::uuid
  |]
 where
   encodePayload ( fromUUID -> uid
                 , fromText -> name
                 , fromText -> surname
                 , fromText -> login
                 , (fromUUID <$>) -> mAvatar
                 , fromUTCTime -> createdAt
                 , fromBool -> privileged
                 ) = Payload uid name surname login mAvatar createdAt privileged
