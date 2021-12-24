{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Auth where

import Hasql.TH
import Hasql.Transaction
import Universum         hiding (toText)

import qualified Types.Users as Users (ID)

import Types.Auth
import Types.Common
import Types.Users
import Hasql.Encoders (timestamptz)

getTokenInfo :: Token -> CurrentTime -> Transaction (Maybe TokenInfo)
getTokenInfo (toText -> mToken) (toUTCTime -> curTime) =
  (encodeTokenInfo <$>) <$> statement mToken
  [maybeStatement| SELECT token::text, privileged::bool, expires::timestamptz
                        , user_id::uuid
                   FROM auth WHERE token=$1::text |]
  where
    encodeTokenInfo ( fromText -> token
                    , fromBool -> privileged
                    , fromBool . (<= curTime) -> expired
                    , fromUUID -> user
                    ) = TokenInfo token privileged expired user


login :: Login -> Hash -> Transaction (Maybe LoginInfo)
login (toText -> login) (toText -> passwordHash) =
  (uncurry LoginInfo . bimap fromUUID fromBool <$>) <$>
  statement (login, passwordHash) [maybeStatement|
    SELECT id::uuid, privileged::bool FROM users
    WHERE login=$1::text AND password_hash=$2::text
                                  |]


issueToken :: ExpirationDate -> IsAdmin -> Token
           -> Users.ID -> Transaction TokenPayload
issueToken (toUTCTime -> expires) (toBool -> privileged) (toText -> token)
  (toUUID -> user) = uncurry TokenPayload . bimap fromText fromUTCTime <$>
  statement (user, privileged, token, expires) [singletonStatement|
    INSERT INTO auth (user_id,privileged,token,expires)
    VALUES ($1::uuid,$2::bool,$3::text,$4::timestamptz)
    ON CONFLICT ON CONSTRAINT auth_pkey
    DO UPDATE SET token=$3::text,expires=$4::timestamptz
    RETURNING token::text,expires::timestamptz
                                               |]
