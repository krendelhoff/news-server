{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Server.Users where

import Universum hiding (get)

import Router
import Types.Router
import Types.Users

import qualified Types.Users as Users (ID)

type API = "users" :> (CreateAPI :<|> RequireUser :> GetAPI)

server :: Server API
server = create :<|> get

type CreateAPI = "create" :> ReqBody CreateForm :> Post TokenPayload

create :: Server CreateAPI
create = undefined

type GetAPI = Get Payload

get :: Server GetAPI
get = undefined
