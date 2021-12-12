{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Server.Users where

import Universum hiding (get)

import qualified Types.Users as Users

import DataModel

type API = CreateAPI :<|> GetAPI

usersServer :: Server API
usersServer = create :<|> get

type CreateAPI = "create"
               :> ReqBody Users.CreateForm
               :> Post Users.CreatePayload

create :: Server CreateAPI
create = undefined

type GetAPI = Get Users.Payload

get :: Server GetAPI
get = undefined
