{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
module Types.Users where

import Data.Aeson
import Deriving.Aeson.Stock
import Universum

import Types.TH

newUUIDType "ID"
newTextType "Name"
newTextType "Token"
newTextType "Hash"
newTextType "Surname"
newTextType "Login"
newByteaType "Picture"
newBoolType "IsAdmin"
newUTCTimeType "CreationTime"
newTextType "Password"
newUTCTimeType "ExpirationDate"

data CreateForm = CreateForm
  { createFormName       :: Name
  , createFormSurname    :: Surname
  , createFormLogin      :: Login
  , createFormAvatar     :: Picture
  , createFormPassword   :: Password
  , createFormCreatedAt  :: CreationTime
  , createFormPrivigeded :: IsAdmin
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via Prefixed "createForm" CreateForm

data TokenPayload = TokenPayload
  { tokenPayloadToken   :: Token
  , tokenPayloadExpires :: ExpirationDate
  } deriving (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via Prefixed "tokenPayload" TokenPayload
-- TODO TH FOR THIS, too verbose and boilerplate

data Payload = Payload
  { payloadId         :: ID
  , payloadName       :: Name
  , payloadSurname    :: Surname
  , payloadLogin      :: Login
  , payloadAvatar     :: Picture
  , payloadPassword   :: Password
  , payloadCreatedAt  :: CreationTime
  , payloadPrivigeded :: IsAdmin
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via Prefixed "payload" Payload
