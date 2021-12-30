{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Universum

import Infrastructure
import Types.Environment (Server)

import qualified Server.Auth       as Auth
import qualified Server.Authors    as Authors
import qualified Server.Categories as Categories
import qualified Server.Pictures   as Pictures
import qualified Server.Users      as Users

type API = Auth.API :<|> Users.API
                    :<|> Pictures.API
                    :<|> Authors.API
                    :<|> Categories.API

server :: Server API
server = Auth.server :<|> Users.server
                     :<|> Pictures.server
                     :<|> Authors.server
                     :<|> Categories.server
