{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Authors where

import Universum hiding (toText)

import Infrastructure
import Types.Authors

import qualified Types.Users as Users

decodePayload :: (UUID, Text) -> Payload
decodePayload = uncurry Payload . bimap fromUUID fromText

updateUnsafe :: ID -> Description -> Transaction Payload
updateUnsafe (toUUID -> uid) (toText -> desc) =
  decodePayload <$> statement (uid, desc)
  [singletonStatement|
     UPDATE authors SET description=$2::text
     WHERE user_id = $1::uuid
     RETURNING user_id::uuid,description::text
  |]

get :: ID -> Transaction (Maybe Payload)
get (toUUID -> aid) =
  (decodePayload <$>) <$> statement aid [maybeStatement|
    SELECT user_id::uuid,description::text FROM authors
    WHERE user_id=$1::uuid
                                        |]

downgrade :: ID -> Transaction ()
downgrade (toUUID -> aid) =
  statement aid
  [resultlessStatement| DELETE FROM authors WHERE user_id=$1::uuid |]

promoteUnsafe :: Users.ID -> Description -> Transaction Payload
promoteUnsafe (toUUID -> uid) (toText -> desc) =
  decodePayload <$> statement (uid, desc)
  [singletonStatement|
     INSERT INTO authors (user_id, description)
     VALUES ($1::uuid, $2::text)
     RETURNING user_id::uuid,description::text
  |]
