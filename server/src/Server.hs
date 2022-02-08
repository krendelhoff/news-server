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
import Control.Monad.Except (MonadError, throwError)

import Infrastructure
import Types.Auth
import Types.Environment

import qualified Types.Users as Users

import qualified Server.Auth  as Auth
import qualified Server.User  as User
import qualified Server.Admin as Admin


type API = Auth.API :<|> (AuthUser AuthData  :> RefreshAPI)
                    :<|> (AuthUser AuthData  :> User.API)
                    :<|> (AuthAdmin AuthData :> Admin.API)

server :: ServerT API App
server = Auth.server :<|> authRefreshServer :<|> authUserServer :<|> authAdminServer

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

instance FromAuth AuthData where
  fromRawAuth (RawAuthData uid token rToken iA authT) =
    AuthData (fromUUID uid) (fromText token) (fromText rToken) (fromBool iA) authT

authUserServer :: AuthResult AuthData -> ServerT User.API App
authUserServer NotFound                                  = throwAll err401
authUserServer TokenExpired                              = throwAll err403TokenExpired
authUserServer (AuthSuccess (AuthData uid _ _ _ Access)) = runAuthenticated uid User.server
authUserServer _ = throwAll err403

authAdminServer :: AuthResult AuthData -> ServerT Admin.API App
authAdminServer (AuthSuccess (AuthData uid _ _ (toBool -> True) Access)) = runAuthenticated uid Admin.server
authAdminServer _                                                        = throwAll err404

type RefreshAPI = Auth.RefreshAPI

authRefreshServer :: AuthResult AuthData -> ServerT RefreshAPI App
authRefreshServer (AuthSuccess (AuthData uid _ _ _ Refresh)) = runAuthenticated uid Auth.refresh
authRefreshServer _                                          = throwAll err401
