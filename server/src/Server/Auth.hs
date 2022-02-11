{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Server.Auth where

import Hasql.Pool (Pool)
import Data.Coerce (coerce)
import Universum   hiding (toText)

import Effects
import Infrastructure
import Types.Auth
import Types.Environment (App, AuthenticatedApp, HasUserId(getUserId))
import Server.Errors
import Types.Logger      (Level(INFO))
import Types.Users       (CreateForm(CreateForm))
import Utils             (hash)

import qualified Effects.Utils as Utils
import qualified Utils         as RawUtils

import qualified Database.Auth as DB

type API = HelloAPI :<|> "auth" :> (RegisterAPI :<|> LoginAPI)

server :: ServerT API App
server = hello :<|> register :<|> login

type HelloAPI = Get Messages

hello :: App Messages
hello = return $ coerce @[Text] @Messages ["Hello, World!"]

type RegisterAPI = "register" :> ReqBody 'JSON CreateForm :> Post AuthPayload

register :: CreateForm -> App AuthPayload
register (CreateForm name surname login mAvatar password) =
  run (DB.login login password) >>= \case
    Nothing -> do
      tokens         <- Utils.generateTokens
      expirationDate <- Utils.getExpirationDate
      correct        <- run (DB.pictureCorrect mAvatar)
      if correct
      then do
        run (DB.create name surname login mAvatar password)
          >>= run . DB.issueToken expirationDate (fromBool False) tokens
      else reject pictureNotExistsError
    _       -> reject userAlreadyExistsError

type LoginAPI = "login" :> ReqBody 'JSON LoginForm :> Post AuthPayload

login :: LoginForm -> App AuthPayload
login (LoginForm userLogin password) = run (DB.login userLogin password) >>= \case
    Nothing -> reject userNotFoundError
    Just (LoginInfo user rights) -> do
      exprDate <- Utils.getExpirationDate
      log INFO $ "User " <> toText userLogin <> " made login"
      Utils.generateTokens >>= run . ($ user) . DB.issueToken exprDate rights

type RefreshAPI = "auth" :> "refresh" :> Get AuthPayload

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
