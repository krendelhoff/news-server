{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Universum

import Types.Environment (Server)
import Types.Router

import qualified Server.Auth     as Auth
import qualified Server.Pictures as Pictures
import qualified Server.Users    as Users

type API = Auth.API :<|> Users.API
                    :<|> Pictures.API

server :: Server API
server = Auth.server
    :<|> Users.server
    :<|> Pictures.server
