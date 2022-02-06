{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Server where

import Universum

import Infrastructure
import Types.Auth
import Types.Environment

import qualified Types.Users as Users

import qualified Server.Auth            as Auth
import qualified Server.User.Categories as Categories
--import qualified Server.Admin.Authors   as Authors
--import qualified Server.Authors    as Authors
--import qualified Server.Categories as Categories
--import qualified Server.Pictures   as Pictures
--import qualified Server.Users      as Users

type UserAPI = AuthUser :> Categories.API

--type AdminAPI = AuthAdmin :> (Categories.API)

type API = Auth.API :<|> UserAPI
 --                   :<|> AdminAPI

class UnliftAuthenticated authApp app | authApp -> app where
  runAuthenticated :: Users.ID -> authApp -> app

instance UnliftAuthenticated authApp app => UnliftAuthenticated (a -> authApp) (a -> app) where
  runAuthenticated uid authAppF = runAuthenticated uid . authAppF

instance (UnliftAuthenticated authApp1 app1, UnliftAuthenticated authApp2 app2
          ) => UnliftAuthenticated (authApp1 :<|> authApp2) (app1 :<|> app2) where
  runAuthenticated uid (app1 :<|> app2) = runAuthenticated uid app1 :<|> runAuthenticated uid app2

instance UnliftAuthenticated (AuthenticatedApp a) (App a) where
  runAuthenticated uid authApp = runReaderT (runAuthApp authApp) uid

userServer :: ServerT UserAPI AuthenticatedApp
userServer = Categories.server

authenticatedServer :: AuthResult LoginInfo -> ServerT UserAPI App
authenticatedServer NotFound = undefined -- throwError err401 надо точно так же как выше сделать инстансы
authenticatedServer TokenExpired = undefined -- throwError err403TokenExpired
authenticatedServer (AuthSuccess (LoginInfo uid isAdmin)) = runAuthenticated uid userServer

server :: ServerT API App
server = Auth.server :<|> authenticatedServer
