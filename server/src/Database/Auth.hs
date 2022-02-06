{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Database.Auth where

import Hasql.TH
import Hasql.Transaction
import Universum         hiding (toText)

import qualified Types.Users as Users

import Types.Auth
import Types.Users (ExpirationDate, IsAdmin, Login, Password)
import Infrastructure
import Types.Utils (CurrentTime)
import Utils       (hash)

getByRefreshToken :: RefreshToken -> Transaction (AuthResult RawAuthData)
getByRefreshToken (toText -> mToken) =
  statement mToken [maybeStatement|
    SELECT user_id::uuid, token::text, refresh_token::text, privileged::bool
    FROM auth WHERE refresh_token = $1::text
                   |] >>= \case
                       Nothing -> return NotFound
                       Just x  -> return $ AuthSuccess $ decodeRawAuthData x
 where
   decodeRawAuthData (u, t, rT, iA) = RawAuthData u t rT iA

getByAccessToken :: CurrentTime -> AccessToken -> Transaction (AuthResult RawAuthData)
getByAccessToken curTime mToken = do
  getTokenInfo mToken curTime >>= \case
    Nothing                       -> return NotFound
    Just (TokenInfo _ _ (toBool -> True) _ _) -> return TokenExpired
    Just (TokenInfo token isAdmin _ refreshToken user) ->
      return $ AuthSuccess $ RawAuthData (toUUID user) (toText token) (toText refreshToken) (toBool isAdmin)

getTokenInfo :: AccessToken -> CurrentTime -> Transaction (Maybe TokenInfo)
getTokenInfo (toText -> mToken) (toUTCTime -> curTime) =
  (encodeTokenInfo <$>) <$> statement mToken
  [maybeStatement| SELECT token::text, privileged::bool, expires::timestamptz
                        , refresh_token::text, user_id::uuid
                   FROM auth WHERE token = $1::text |]
  where
    encodeTokenInfo ( fromText -> token
                    , fromBool -> privileged
                    , fromBool . (<= curTime) -> expired
                    , fromText -> refreshToken
                    , fromUUID -> user
                    ) = TokenInfo token privileged expired refreshToken user

login :: Login -> Password -> Transaction (Maybe LoginInfo)
login (toText -> login) (toText . hash -> passwordHash) =
  (uncurry LoginInfo . bimap fromUUID fromBool <$>) <$>
  statement (login, passwordHash) [maybeStatement|
    SELECT id::uuid, privileged::bool FROM users
    WHERE login=$1::text AND password_hash=$2::text
                                  |]

issueToken :: ExpirationDate -> IsAdmin -> (AccessToken, RefreshToken)
           -> Users.ID -> Transaction AuthPayload
issueToken (toUTCTime -> expires) (toBool -> privileged)
  (bimap toText toText -> (token, refreshToken)) (toUUID -> user) =
  encodeAuthPayload <$>
  statement (user, privileged, token, refreshToken, expires)
  [singletonStatement|
    INSERT INTO auth (user_id,privileged,token,refresh_token,expires)
    VALUES ($1::uuid,$2::bool,$3::text,$4::text,$5::timestamptz)
    ON CONFLICT ON CONSTRAINT auth_pkey
    DO UPDATE SET token=$3::text,refresh_token=$4::text,expires=$5::timestamptz
    RETURNING token::text,expires::timestamptz,refresh_token::text
 |]
 where
   encodeAuthPayload ( fromText    -> at
                     , fromUTCTime -> exprs
                     , fromText    -> rt
                     ) = AuthPayload at rt exprs
