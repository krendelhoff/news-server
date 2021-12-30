{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Universum

import Types.Environment (Server)
import Infrastructure

import qualified Server.Auth     as Auth
import qualified Server.Pictures as Pictures
import qualified Server.Users    as Users
import qualified Server.Authors  as Authors

type API = Auth.API :<|> Users.API
                    :<|> Pictures.API
                    :<|> Authors.API

server :: Server API
server = Auth.server
    :<|> Users.server
    :<|> Pictures.server
    :<|> Authors.server
