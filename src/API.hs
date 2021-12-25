{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Universum

import Router
import Types.Router

import qualified Server.Auth  as Auth
import qualified Server.Users as Users
import qualified Server.Pictures as Pictures

type API = Auth.API :<|> Users.API
      :<|> RequireUser :> Pictures.API
           
server :: Server API
server = Auth.server
    :<|> Users.server
    :<|> Pictures.server
