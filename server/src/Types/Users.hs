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

import Control.Lens.TH (makeFields)
import Universum

import Types.Infrastructure

import qualified Types.Pictures as Pictures

newUUIDType "ID"
newTextType "Name"
newTextType "Password"
newTextType "Surname"
newTextType "Hash"
newTextType "Login"

newBoolType "IsAdmin"
newUTCTimeType "CreationTime"
newUTCTimeType "ExpirationDate"

data CreateForm = CreateForm
  { createFormName     :: Name
  , createFormSurname  :: Surname
  , createFormLogin    :: Login
  , createFormAvatar   :: Maybe Pictures.ID
  , createFormPassword :: Password
  }
deriveWeb "createForm" ''CreateForm
makeFields ''CreateForm

data Payload = Payload
  { payloadUserId     :: ID
  , payloadName       :: Name
  , payloadSurname    :: Surname
  , payloadLogin      :: Login
  , payloadAvatar     :: Maybe Pictures.ID
  , payloadCreatedAt  :: CreationTime
  , payloadPrivileged :: IsAdmin
  }
deriveWeb "payload" ''Payload
makeFields ''Payload
