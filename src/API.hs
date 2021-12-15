{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Universum

import Router
import Types.Router

import qualified Server.Auth  as Auth
import qualified Server.Users as Users

data Mock1
data Mock2
data Mock3

type API = Auth.API :<|> Users.API
           
server :: Server API
server = Auth.server :<|> Users.server
