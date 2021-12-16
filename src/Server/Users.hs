{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE BlockArguments #-}
module Server.Users where

import Control.Monad.Except
import Network.HTTP.Types
import Data.Aeson
import Universum            hiding (get)

import Common
import DB           (run)
import Router
import Types.Common
import Types.Router
import Types.Users

import qualified Database.Auth as Auth
import qualified Database.Users as Users
import qualified Types.Users   as Users (ID)

type API = "users" :> (CreateAPI :<|> RequireUser :> GetAPI)

server :: Server API
server = create :<|> get

-- TODO all statuses and throwError to dedicated namespace

type CreateAPI = "create" :> ReqBody CreateForm :> Post TokenPayload

create :: Server CreateAPI
create (CreateForm name surname login mAvatar password) = do
  let passwordHash = hash password
  run (Auth.login login passwordHash) >>= \case
    Nothing -> do
      accessToken <- generateToken
      expirationDate <- getExpirationDate
      run (Users.create name surname login mAvatar passwordHash
             >>= Auth.issueToken expirationDate (fromBool False) accessToken)
    _       -> throwError (mkError status403 "User already exists")


type GetAPI = Get Payload

get :: Server GetAPI
get = undefined
