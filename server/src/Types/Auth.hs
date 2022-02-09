{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
module Types.Auth where

import Universum (Eq, Show)

import Types.Infrastructure (deriveWeb, newBoolType, newTextType, AuthType)
import Types.Users          (ExpirationDate, IsAdmin, Login, Password)

import qualified Types.Users as Users

newBoolType "IsExpired"
newTextType "AccessToken"
newTextType "RefreshToken"

data TokenInfo = TokenInfo { tokenInfoToken   :: AccessToken
                           , tokenInfoAdmin   :: IsAdmin
                           , tokenInfoExpired :: IsExpired
                           , tokenInfoRefresh :: RefreshToken
                           , tokenInfoId      :: Users.ID
                           } deriving (Eq, Show)

data LoginForm = LoginForm
  { loginFormLogin    :: Login
  , loginFormPassword :: Password
  }
deriveWeb "loginForm" ''LoginForm

data LoginInfo = LoginInfo
  { loginInfoUserId  :: Users.ID
  , loginInfoIsAdmin :: IsAdmin
  } deriving (Eq, Show)

data AuthPayload = AuthPayload
  { authPayloadToken        :: AccessToken
  , authPayloadRefreshToken :: RefreshToken
  , authPayloadExpires      :: ExpirationDate
  }
deriveWeb "authPayload" ''AuthPayload

data AuthData = AuthData
  { authDataUserId       :: Users.ID
  , authDataToken        :: AccessToken
  , authDataRefreshToken :: RefreshToken
  , authDataIsAdmin      :: IsAdmin
  , authDataAuthType     :: AuthType
  }
deriveWeb "authData" ''AuthData

data Rights = Admin | User
