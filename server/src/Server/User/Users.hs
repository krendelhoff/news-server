{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Server.User.Users where

import Universum hiding (get)

import Application.Effects
import Infrastructure
import Types.Auth          (AuthPayload)
import Types.Users         (CreateForm(CreateForm), Payload)

import qualified Application.Effects.Users as Users
import qualified Application.Effects.Utils as Utils


--type API = "users" :> (CreateAPI)-- :<|> GetAPI)
--
--server :: Server API
--server = unde create-- :<|> get
--
--
--
--type GetAPI = Get Payload
--
--get :: AcquireUser m => m Payload
--get = undefined --Users.get
