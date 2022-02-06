{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Server.Auth where

import Universum hiding (toText)

import Application.Effects
import Infrastructure
import Types.Auth
import Types.Users
import Types.Environment
import Types.Logger        (Level(INFO))
import Utils               (hash)

import qualified Application.Effects.Auth  as Auth
import qualified Application.Effects.Utils as Utils

server :: ServerT API App
server = create

type CreateAPI = "create" :> ReqBody 'JSON CreateForm :> Post AuthPayload

type API = "auth" :> CreateAPI
--create :: (PersistUser m, UsesCurrentTime m, GenRandom m, CanReject m
--           ) => CreateForm -> m AuthPayload
create :: CreateForm -> App AuthPayload
create (CreateForm name surname login mAvatar password) = undefined {- do
  Auth.login login password >>= \case
    Nothing -> do
      tokens <- Utils.generateTokens
      expirationDate <- Utils.getExpirationDate
      Users.create name surname login mAvatar password
        >>= Auth.issueToken expirationDate (fromBool False) tokens
    _       -> reject (mkError status403 "User already exists")
-}
--
--type API = "auth" :> (LoginAPI :<|> RefreshAPI)
--
--server :: Server API
--server = login :<|> refresh
--
--type LoginAPI = "login" :> ReqBody 'JSON LoginForm :> Post AuthPayload
--
--login :: (Auth m, UsesCurrentTime m, Logging m, GenRandom m, CanReject m
--          ) => LoginForm -> m AuthPayload
--login (LoginForm login password) = do
--  Auth.login login password >>= \case
--    Nothing -> reject (mkError status401 "User not found")
--    Just (LoginInfo user rights) -> do
--      exprDate <- Utils.getExpirationDate
--      log INFO $ "User " <> toText login <> " made login"
--      Utils.generateTokens >>= ($ user) . Auth.issueToken exprDate rights
--
--
--type RefreshAPI = "refresh" :> Capture "refresh_token" RefreshToken
--                            :> Get AuthPayload
--
--refresh :: (CanReject m, AuthenticateWeak m, UsesCurrentTime m
--            , GenRandom m) => RefreshToken -> m AuthPayload
--refresh rt = do
--  TokenInfo _ rights _ mRToken user <- Auth.getTokenInfo
--  if rt /= mRToken
--  then reject err403
--  else do
--    exprDate <- Utils.getExpirationDate
--    Utils.generateTokens >>= ($ user) . Auth.issueToken exprDate rights
