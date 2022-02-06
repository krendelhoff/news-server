{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}
module Server where

import Universum

import Infrastructure
import Types.Auth
import Types.Environment

import qualified Types.Users as Users

import qualified Server.Admin.Authors   as Authors
import qualified Server.Auth            as Auth
import qualified Server.User.Categories as Categories
--import qualified Server.Authors    as Authors
--import qualified Server.Categories as Categories
--import qualified Server.Pictures   as Pictures
--import qualified Server.Users      as Users
import Control.Monad.Except (MonadError, throwError)
import Data.Coerce          (coerce)
import Data.UUID            (nil)

type UserAPI = Auth.RefreshAPI :<|> Categories.API

type AdminAPI = Authors.API

type API = Auth.API :<|> (AuthUser AuthData  :> UserAPI)
                    :<|> (AuthAdmin AuthData :> AdminAPI)

class ThrowAll a where
  throwAll :: ServerError -> a

instance (ThrowAll a, ThrowAll b) => ThrowAll (a :<|> b) where
  throwAll e = throwAll e :<|> throwAll e

instance {-# OVERLAPPABLE #-} MonadError ServerError m => ThrowAll (m a) where
  throwAll = throwError

instance {-# OVERLAPPING #-} ThrowAll b => ThrowAll (a -> b) where
  throwAll e = const $ throwAll e

class Unlift authApp app | authApp -> app where
  runAuthenticated :: Users.ID -> authApp -> app

instance Unlift authApp app => Unlift (a -> authApp) (a -> app) where
  runAuthenticated uid authAppF = runAuthenticated uid . authAppF

instance (Unlift authApp1 app1, Unlift authApp2 app2
          ) => Unlift (authApp1 :<|> authApp2) (app1 :<|> app2) where
  runAuthenticated uid (app1 :<|> app2) = runAuthenticated uid app1 :<|> runAuthenticated uid app2

instance Unlift (AuthenticatedApp rights a) (App a) where
  runAuthenticated uid authApp = runReaderT (runAuthApp authApp) uid

userServer :: ServerT UserAPI (AuthenticatedApp '[User])
userServer = Auth.refresh :<|> Categories.server

instance FromAuth AuthData where
  fromRawAuth (RawAuthData uid token rToken iA) =
    AuthData (fromUUID uid) (fromText token) (fromText rToken) (fromBool iA)

authUserServer :: AuthResult AuthData -> ServerT UserAPI App
authUserServer NotFound = throwAll err401
authUserServer TokenExpired = throwAll err403TokenExpired
authUserServer (AuthSuccess (AuthData uid _ _ _)) = runAuthenticated uid userServer

adminServer :: ServerT AdminAPI (AuthenticatedApp '[Admin, User])
adminServer = Authors.server

authAdminServer :: AuthResult AuthData -> ServerT AdminAPI App
authAdminServer (AuthSuccess (AuthData uid _ _ (toBool -> True))) = runAuthenticated uid adminServer
authAdminServer _ = throwAll err404

server :: ServerT API App
server = Auth.server :<|> authUserServer :<|> authAdminServer
