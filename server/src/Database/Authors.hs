{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Authors where

import Universum hiding (toText)

import Infrastructure
import Types.Authors

import qualified Types.Users as Users

encodePayload = uncurry Payload . bimap fromUUID fromText

update :: ID -> Description -> Transaction Payload
update (toUUID -> uid) (toText -> desc) =
  encodePayload <$> statement (uid, desc)
  [singletonStatement|
     UPDATE authors SET description=$2::text
     WHERE user_id = $1::uuid
     RETURNING user_id::uuid,description::text
  |]

get :: ID -> Transaction (Maybe Payload)
get (toUUID -> aid) =
  (encodePayload <$>) <$> statement aid [maybeStatement|
    SELECT user_id::uuid,description::text FROM authors
    WHERE user_id=$1::uuid
                                        |]

downgrade :: ID -> Transaction NoContent
downgrade (toUUID -> aid) =
  NoContent <$ statement aid
  [resultlessStatement| DELETE FROM authors WHERE user_id=$1::uuid |]
