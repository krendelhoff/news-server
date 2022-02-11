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
import Data.Time (UTCTime)

getUnsafe :: Users.ID -> Transaction Payload
getUnsafe (toUUID -> uid) = decodePayload <$> statement uid
  [singletonStatement|
     SELECT id::uuid, name::text
          , surname::text, login::text
          , avatar::uuid?, created_at::timestamptz
          , privileged::bool
     FROM users
     WHERE id = $1::uuid
  |]

decodePayload :: (UUID, Text, Text, Text, Maybe UUID, UTCTime, Bool) -> Payload
decodePayload ( fromUUID -> uid
              , fromText -> name
              , fromText -> surname
              , fromText -> login
              , (fromUUID <$>) -> mAvatar
              , fromUTCTime -> createdAt
              , fromBool -> privileged
              ) = Payload uid name surname login mAvatar createdAt privileged

get :: Users.ID -> Transaction (Maybe Payload)
get (toUUID -> uid) = (decodePayload <$>) <$> statement uid
  [maybeStatement|
     SELECT id::uuid, name::text
          , surname::text, login::text
          , avatar::uuid?, created_at::timestamptz
          , privileged::bool
     FROM users
     WHERE id = $1::uuid
  |]

delete :: Users.ID -> Transaction ()
delete (toUUID -> uid) = do
  statement uid [resultlessStatement|
    DELETE FROM auth WHERE user_id = $1::uuid
                |]
  statement uid [resultlessStatement|
  DELETE FROM users WHERE id = $1::uuid
                |]

