{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
module Types.Users where

import Data.Aeson
import Universum
import Deriving.Aeson.Stock

import Types.TH

newUUIDType "ID"
newTextType "Name"
newTextType "Token"
newTextType "Surname"
newTextType "Login"
newByteaType "Picture"
newBoolType "IsAdmin"
newUTCTimeType "CreationTime"
newTextType "Password"

data CreateForm = CreateForm
  { createFormName         :: Name
  , createFormSurname      :: Surname
  , createFormLogin        :: Login
  , createFormAvatar       :: Picture
  , createFormPassword     :: Password
  , createFormCreatedAt    :: CreationTime
  , createFormPrivigeded   :: IsAdmin
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via Prefixed "createForm" CreateForm

newtype CreatePayload = CreatePayload
  { createPayloadToken :: Token
  } deriving (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via Prefixed "createPayload" CreatePayload

data Payload = Payload
  { payloadId           :: ID
  , payloadName         :: Name
  , payloadSurname      :: Surname
  , payloadLogin        :: Login
  , payloadAvatar       :: Picture
  , payloadPassword     :: Password
  , payloadCreatedAt    :: CreationTime
  , payloadPrivigeded   :: IsAdmin
  } deriving stock (Eq, Show, Generic)
    deriving (FromJSON, ToJSON) via Prefixed "payload" Payload
