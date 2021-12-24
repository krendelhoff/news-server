{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Server.Users where

import Data.Aeson
import Universum  hiding (get)

import Application.Effects.Users (Users)
import Common
import DB                        (run)
import Router
import Types.Auth
import Types.Environment
import Types.Router
import Types.Users

import qualified Application.Effects.Users as Users
import qualified Database.Auth             as Auth
import qualified Database.Users            as RawDBUsers
import qualified Types.Users               as Users (ID)

type API = "users" :> (CreateAPI :<|> RequireUser :> GetAPI)

server :: Server API
server = create :<|> get

type CreateAPI = "create" :> ReqBody 'JSON CreateForm :> Post TokenPayload

create :: Server CreateAPI
create (CreateForm name surname login mAvatar password) = do
  let passwordHash = hash password
  run (Auth.login login passwordHash) >>= \case
    Nothing -> do
      accessToken <- generateToken
      expirationDate <- getExpirationDate
      run (RawDBUsers.create name surname login mAvatar passwordHash
             >>= Auth.issueToken expirationDate (fromBool False) accessToken)
    _       -> throwError (mkError status403 "User already exists")


type GetAPI = Get Payload

get :: (MonadReader env m, HasUserId env Users.ID, Users m) => m Payload
get = view userId >>= Users.get
