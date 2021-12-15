{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
module Server.Auth where

import Universum

import qualified Database.Auth as Auth

import Router
import Types.Auth
import Types.Router
import Types.Users
import Common
import DB (run)
import Network.HTTP.Types.Status
import Control.Monad.Except (throwError)

type API = "auth" :> (LoginAPI :<|> RequireUser :> RefreshAPI)

server :: Server API
server = login :<|> refresh

type LoginAPI = "login" :> ReqBody LoginForm :> Post TokenPayload

login :: Server LoginAPI
login (LoginForm login password) = do
  run (Auth.login login $ hash password) >>= \case
    Nothing -> throwError (mkError status401 "User not found")
    Just (LoginInfo user rights) -> do
      exprDate <- getCurrentTime <&> getExpirationDate
      generateToken >>= run . Auth.issueToken exprDate user rights


type RefreshAPI = "refresh" :> Post TokenPayload

refresh :: Server RefreshAPI
refresh = undefined
