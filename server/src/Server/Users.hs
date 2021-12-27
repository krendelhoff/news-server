{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Server.Users where

import Universum hiding (get)

import Application.Effects
import Infrastructure
import Types.Auth          (AuthPayload)
import Types.Environment   (Server)
import Types.Users         (CreateForm(CreateForm), Payload)

import qualified Application.Effects.Auth  as Auth
import qualified Application.Effects.Users as Users
import qualified Application.Effects.Utils as Utils


type API = "users" :> (CreateAPI :<|> GetAPI)

server :: Server API
server = create :<|> get

type CreateAPI = "create" :> ReqBody 'JSON CreateForm :> Post AuthPayload

create :: (PersistUser m, UsesCurrentTime m, GenRandom m, Auth m, CanReject m
           ) => CreateForm -> m AuthPayload
create (CreateForm name surname login mAvatar password) = do
  Auth.login login password >>= \case
    Nothing -> do
      tokens <- Utils.generateTokens
      expirationDate <- Utils.getExpirationDate
      Users.create name surname login mAvatar password
        >>= Auth.issueToken expirationDate (fromBool False) tokens
    _       -> reject (mkError status403 "User already exists")


type GetAPI = Get Payload

get :: AcquireUser m => m Payload
get = Users.get
