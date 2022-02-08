{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
module Server.Admin where

import Universum

import Infrastructure
import Types.Environment
import Types.Auth

import qualified Server.Admin.Categories as Categories
import qualified Server.Admin.Authors    as Authors

type API = Authors.API :<|> Categories.API

server :: ServerT API (AuthenticatedApp '[Admin, User])
server = Authors.server :<|> Categories.server
