{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Server.Auth where

import Hasql.Pool (Pool)
import Universum  hiding (toText)

import Infrastructure
import Types.Auth
import Effects
import Types.Environment (App, AuthenticatedApp, HasUserId (getUserId))
import Types.Logger      (Level(INFO))
import Types.Users       (CreateForm(CreateForm))
import Utils             (hash)

import qualified Effects.Utils as Utils
import qualified Utils         as RawUtils

import qualified Database.Auth as DB


type API = "auth" :> (CreateAPI :<|> LoginAPI)

server :: ServerT API App
server = create :<|> login

type CreateAPI = "create" :> ReqBody 'JSON CreateForm :> Post AuthPayload

create :: CreateForm -> App AuthPayload
create (CreateForm name surname login mAvatar password) =
  run (DB.login login password) >>= \case
    Nothing -> do
      tokens         <- Utils.generateTokens
      expirationDate <- Utils.getExpirationDate
      run (DB.create name surname login mAvatar password)
        >>= run . DB.issueToken expirationDate (fromBool False) tokens
    _       -> reject (mkError status403 "User already exists")

type LoginAPI = "login" :> ReqBody 'JSON LoginForm :> Post AuthPayload

login :: LoginForm -> App AuthPayload
login (LoginForm userLogin password) = run (DB.login userLogin password) >>= \case
    Nothing -> reject (mkError status401 "User not found")
    Just (LoginInfo user rights) -> do
      exprDate <- Utils.getExpirationDate
      log INFO $ "User " <> toText userLogin <> " made login"
      Utils.generateTokens >>= run . ($ user) . DB.issueToken exprDate rights

type RefreshAPI = "refresh" :> Get AuthPayload

refresh :: AuthenticatedApp '[User] AuthPayload
refresh = do
  exprDate <- Utils.getExpirationDate
  user     <- getUserId
  Utils.generateTokens >>= run . DB.refreshToken user exprDate

authenticate :: (HasPool env Pool, MonadReader env m, MonadIO m, MonadThrow m
                 ) => Text -> m (AuthResult RawAuthData)
authenticate mToken = do
  curTime <- RawUtils.getCurrentTime
  run (DB.getByAccessToken curTime (fromText mToken)) >>= \case
    NotFound -> run $ DB.getByRefreshToken $ fromText mToken
    x        -> return x
