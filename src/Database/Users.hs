{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Users where

import Hasql.Transaction
import Hasql.TH
import Hasql.Encoders (timestamptz)
import Universum         hiding (toText)

import Types.Common
import Types.Users

import qualified Types.Users as Users (ID)
import qualified Types.Pictures as Pictures


create :: Name -> Surname -> Login -> Maybe Pictures.ID
       -> Hash -> Transaction Users.ID
create (toText -> name) (toText -> surname)
       (toText -> login) ((toUUID <$>) -> mAvatar)
       (toText -> passwordHash) = fromUUID <$>
  statement (name,surname,login, mAvatar, passwordHash)
  [singletonStatement|
     INSERT INTO users (name,surname,login,avatar,password_hash,privileged)
     VALUES ($1::text,$2::text,$3::text,$4::uuid?,$5::text,false)
     RETURNING id::uuid
  |]

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
