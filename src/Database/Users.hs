{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Users where

import Hasql.Transaction
import Universum         hiding (toText)

import Types.Common
import Types.Users

import qualified Types.Users as Users (ID)
import Hasql.TH

create :: Name -> Surname -> Login -> Maybe Picture
       -> Hash -> Transaction Users.ID
create (toText -> name) (toText -> surname)
       (toText -> login) ((toByteString <$>) -> mAvatar)
       (toText -> passwordHash) = fromUUID <$>
  statement (name,surname,login, mAvatar, passwordHash)
  [singletonStatement|
     INSERT INTO users (name,surname,login,avatar,password_hash,privileged)
     VALUES ($1::text,$2::text,$3::text,$4::bytea?,$5::text,false)
     RETURNING id::uuid
  |]
