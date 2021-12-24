{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
module Types.Users where

import Control.Lens
import Data.Aeson
import Deriving.Aeson.Stock
import Universum

import Deriving.Aeson
import Types.Common

newUUIDType "ID"
newTextType "Surname"
newTextType "Hash"
newTextType "Login"
newUUIDType "PictureID"
newBoolType "IsAdmin"
newUTCTimeType "CreationTime"
newUTCTimeType "ExpirationDate"

data CreateForm = CreateForm
  { createFormName     :: Name
  , createFormSurname  :: Surname
  , createFormLogin    :: Login
  , createFormAvatar   :: Maybe PictureID
  , createFormPassword :: Password
  }
deriveWeb "createForm" ''CreateForm
makeFields ''CreateForm

data TokenPayload = TokenPayload
  { tokenPayloadToken   :: Token
  , tokenPayloadExpires :: ExpirationDate
  }
deriveWeb "tokenPayload" ''TokenPayload

data Payload = Payload
  { payloadUserId     :: ID
  , payloadName       :: Name
  , payloadSurname    :: Surname
  , payloadLogin      :: Login
  , payloadAvatar     :: Maybe PictureID
  , payloadCreatedAt  :: CreationTime
  , payloadPrivileged :: IsAdmin
  }
deriveWeb "payload" ''Payload
makeFields ''Payload
