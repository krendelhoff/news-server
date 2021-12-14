{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Universum

import Router
import Types.Router

import qualified Server.Auth  as Auth
import qualified Server.Users as Users

type API = Auth.API :<|> RequireUser :> Users.API

server :: Server API
server = Auth.server :<|> Users.server
