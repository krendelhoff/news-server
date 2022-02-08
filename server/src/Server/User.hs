{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
module Server.User where

import Infrastructure
import Types.Environment
import Types.Auth

import qualified Server.User.Users      as Users
import qualified Server.User.Pictures   as Pictures
import qualified Server.User.Categories as Categories

type API = Categories.API :<|> Pictures.API :<|> Users.API

server :: ServerT API (AuthenticatedApp '[User])
server = Categories.server :<|> Pictures.server :<|> Users.server
