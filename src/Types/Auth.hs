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

import Data.Aeson
import Deriving.Aeson.Stock
import Universum

import Types.Common
import Types.Users

import qualified Types.Users as Users (ID)

newBoolType "IsExpired"

data TokenInfo = TokenInfo { tokenInfoToken   :: Token
                           , tokenInfoAdmin   :: IsAdmin
                           , tokenInfoExpired :: IsExpired
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
