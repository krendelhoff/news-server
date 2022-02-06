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
import Types.Environment (App, AuthenticatedApp)
import Types.Logger      (Level(INFO))
import Types.Users       (CreateForm(CreateForm))
import Utils             (getCurrentTime, hash)

import qualified Database.Auth as Auth


type API = "auth" :> (CreateAPI :<|> LoginAPI)

server :: ServerT API App
server = create :<|> login

type CreateAPI = "create" :> ReqBody 'JSON CreateForm :> Post AuthPayload

create :: CreateForm -> App AuthPayload
create (CreateForm name surname login mAvatar password) = undefined

type LoginAPI = "login" :> ReqBody 'JSON LoginForm :> Post AuthPayload

login :: LoginForm -> App AuthPayload
login = undefined

type RefreshAPI = "refresh" :> Get AuthPayload

refresh :: AuthenticatedApp '[User] AuthPayload
refresh = undefined

authenticate :: (HasPool env Pool, MonadReader env m, MonadIO m, MonadThrow m
                 ) => Text -> m (AuthResult RawAuthData)
authenticate mToken = do
  curTime <- getCurrentTime
  run (Auth.getByAccessToken curTime (fromText mToken)) >>= \case
    NotFound -> run $ Auth.getByRefreshToken $ fromText mToken
    x        -> return x

-- NOTE effect tracking is actually useless here - it's ok to have such functions in IO

{-create :: (PersistUser m, UsesCurrentTime m, GenRandom m, CanReject m
--           ) => CreateForm -> m AuthPayload
create :: CreateForm -> App AuthPayload
create (CreateForm name surname login mAvatar password) = do
  Auth.login login password >>= \case
    Nothing -> do
      tokens <- Utils.generateTokens
      expirationDate <- Utils.getExpirationDate
      Users.create name surname login mAvatar password
        >>= Auth.issueToken expirationDate (fromBool False) tokens
    _       -> reject (mkError status403 "User already exists")
-}

{-
--login :: (Auth m, UsesCurrentTime m, Logging m, GenRandom m, CanReject m
--          ) => LoginForm -> m AuthPayload
--login (LoginForm login password) = do
--  Auth.login login password >>= \case
--    Nothing -> reject (mkError status401 "User not found")
--    Just (LoginInfo user rights) -> do
--      exprDate <- Utils.getExpirationDate
--      log INFO $ "User " <> toText login <> " made login"
--      Utils.generateTokens >>= ($ user) . Auth.issueToken exprDate rights
-}

{-
--refresh :: (CanReject m, AuthenticateWeak m, UsesCurrentTime m
--            , GenRandom m) => RefreshToken -> m AuthPayload
--refresh rt = do
--  TokenInfo _ rights _ mRToken user <- Auth.getTokenInfo
--  if rt /= mRToken
--  then reject err403
--  else do
--    exprDate <- Utils.getExpirationDate
--    Utils.generateTokens >>= ($ user) . Auth.issueToken exprDate rights
-}
