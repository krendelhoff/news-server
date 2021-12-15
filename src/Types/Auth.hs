{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
module Types.Auth where

import Universum

import Types.TH
import Types.Users

import qualified Types.Users as Users (ID)

import Data.Aeson
import Deriving.Aeson.Stock

newBoolType "IsExpired"

data TokenInfo = TokenInfo { tokenInfoToken   :: Token
                           , tokenInfoAdmin   :: IsAdmin
                           , tokenInfoExpired :: IsExpired
                           , tokenInfoId      :: Users.ID
                           } deriving (Eq, Show)

data LoginForm = LoginForm
  { loginFormLogin    :: Login
  , loginFormPassword :: Password
  } deriving stock (Eq, Show, Generic) -- TODO TH FOR THIS, too verbose and boilerplate
    deriving (FromJSON) via Prefixed "loginForm" LoginForm

data LoginInfo = LoginInfo
  { loginInfoUserId :: Users.ID
  , loginInfoIsAdmin :: IsAdmin
  } deriving (Eq, Show)
