{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Auth where

import Hasql.TH
import Hasql.Transaction
import Universum         hiding (toText)

import qualified Types.Pictures as Pictures
import qualified Types.Users    as Users

import Infrastructure
import Types.Auth
import Types.Users    (ExpirationDate, IsAdmin, Login, Name, Password, Surname)
import Types.Utils    (CurrentTime)
import Utils          (hash)
import Data.Time (UTCTime)


create :: Name -> Surname -> Login -> Maybe Pictures.ID
       -> Password -> Transaction Users.ID
create (toText -> name) (toText -> surname)
       (toText -> login) ((toUUID <$>) -> mAvatar)
       (toText . hash -> passwordHash) = fromUUID <$>
  statement (name,surname,login, mAvatar, passwordHash)
  [singletonStatement|
     INSERT INTO users (name,surname,login,avatar,password_hash,privileged)
     VALUES ($1::text,$2::text,$3::text,$4::uuid?,$5::text,false)
     RETURNING id::uuid
  |]

getByRefreshToken :: RefreshToken -> Transaction (AuthResult RawAuthData)
getByRefreshToken (toText -> mToken) =
  statement mToken [maybeStatement|
    SELECT user_id::uuid, token::text, refresh_token::text, privileged::bool
    FROM auth WHERE refresh_token = $1::text
                   |] >>= \case
                       Nothing -> return NotFound
                       Just x  -> return $ AuthSuccess $ decodeRawAuthData x
 where
   decodeRawAuthData (u, t, rT, iA) = RawAuthData u t rT iA Refresh

getByAccessToken :: CurrentTime -> AccessToken -> Transaction (AuthResult RawAuthData)
getByAccessToken curTime mToken = do
  getTokenInfo mToken curTime >>= \case
    Nothing                       -> return NotFound
    Just (TokenInfo _ _ (toBool -> True) _ _) -> return TokenExpired
    Just (TokenInfo token isAdmin _ refreshToken user) ->
      return $ AuthSuccess $
      RawAuthData (toUUID user) (toText token) (toText refreshToken) (toBool isAdmin) Access

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

refreshToken :: Users.ID -> ExpirationDate
             -> (AccessToken, RefreshToken) -> Transaction AuthPayload
refreshToken (toUUID -> uid) (toUTCTime -> exprDate)
             (bimap toText toText -> (atoken, rtoken)) =
  decodeAuthPayload <$>
  statement (uid, exprDate, atoken, rtoken) [singletonStatement|
    UPDATE auth SET token = $3::text, refresh_token = $4::text
                  , expires = $2::timestamptz
    WHERE user_id = $1::uuid
    RETURNING token::text,expires::timestamptz,refresh_token::text
                                            |]

login :: Login -> Password -> Transaction (Maybe LoginInfo)
login (toText -> login) (toText . hash -> passwordHash) =
  (uncurry LoginInfo . bimap fromUUID fromBool <$>) <$>
  statement (login, passwordHash) [maybeStatement|
    SELECT id::uuid, privileged::bool FROM users
    WHERE login=$1::text AND password_hash=$2::text
                                  |]

decodeAuthPayload :: (Text, UTCTime, Text) -> AuthPayload
decodeAuthPayload ( fromText    -> at
                  , fromUTCTime -> exprs
                  , fromText    -> rt
                  ) = AuthPayload at rt exprs

issueToken :: ExpirationDate -> IsAdmin -> (AccessToken, RefreshToken)
           -> Users.ID -> Transaction AuthPayload
issueToken (toUTCTime -> expires) (toBool -> privileged)
  (bimap toText toText -> (token, refreshToken)) (toUUID -> user) =
  decodeAuthPayload <$>
  statement (user, privileged, token, refreshToken, expires)
  [singletonStatement|
    INSERT INTO auth (user_id,privileged,token,refresh_token,expires)
    VALUES ($1::uuid,$2::bool,$3::text,$4::text,$5::timestamptz)
    ON CONFLICT ON CONSTRAINT auth_pkey
    DO UPDATE SET token=$3::text,refresh_token=$4::text,expires=$5::timestamptz
    RETURNING token::text,expires::timestamptz,refresh_token::text
 |]

pictureCorrect :: Maybe Pictures.ID -> Transaction Bool
pictureCorrect Nothing = return True
pictureCorrect (Just (toUUID -> pid)) = do
  statement pid [singletonStatement|
    SELECT EXISTS (SELECT * FROM pictures where id = $1::uuid)::bool
                |]
