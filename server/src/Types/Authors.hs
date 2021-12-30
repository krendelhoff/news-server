{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Types.Authors where

import Universum

import Types.Infrastructure

import qualified Types.Users as Users

newTextType "Description"
newUUIDType "ID"

data Payload = Payload { payloadUserId      :: ID
                       , payloadDescription :: Description
                       }
deriveWeb "payload" ''Payload

newtype UpdateForm = UpdateForm { updateFormDescription :: Description }
deriveWeb "updateForm" ''UpdateForm
