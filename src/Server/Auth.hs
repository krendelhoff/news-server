{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Server.Auth where

import Universum

import Router
import Types.Auth
import Types.Router
import Types.Users

type API = "auth" :> (LoginAPI :<|> RequireUser :> RefreshAPI)

server :: Server API
server = login :<|> refresh

type LoginAPI = "login" :> ReqBody LoginForm :> Post TokenPayload

login :: Server LoginAPI
login (LoginForm login password) = undefined

type RefreshAPI = "refresh" :> Post TokenPayload

refresh :: Server RefreshAPI
refresh = undefined
