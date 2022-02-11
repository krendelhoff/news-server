{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}
module Server.User.Users where

import Universum hiding (get)

import Effects
import Types.Auth
import Types.Environment
import Infrastructure
import Types.Users         (Payload)

import qualified Effects.Users as Users

type API = "users" :> GetAPI

server :: ServerT API (AuthenticatedApp '[User])
server = get

type GetAPI = Get Payload

get :: AcquireUser m => m Payload
get = Users.get
