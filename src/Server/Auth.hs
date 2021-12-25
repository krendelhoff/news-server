{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Server.Auth where

import Universum hiding (toText)

import qualified Database.Auth as Auth

import Router
import Types.Auth
import Types.Router
import Types.Common
import Types.Users hiding (login)
import Common
import DB (run)
import Network.HTTP.Types.Status
import Control.Monad.Except (throwError)

type API = "auth" :> (LoginAPI :<|> RequireUser :> RefreshAPI)

server :: Server API
server = login :<|> refresh

type LoginAPI = "login" :> ReqBody 'JSON LoginForm :> Post TokenPayload

login :: Server LoginAPI
login (LoginForm login password) = do
  run (Auth.login login $ hash password) >>= \case
    Nothing -> throwError (mkError status401 "User not found")
    Just (LoginInfo user rights) -> do
      exprDate <- getExpirationDate
      log INFO $ "User " <> toText login <> " made login"
      generateToken >>= run . ($ user) . Auth.issueToken exprDate rights


type RefreshAPI = "refresh" :> Post TokenPayload

refresh :: Server RefreshAPI
refresh = undefined
