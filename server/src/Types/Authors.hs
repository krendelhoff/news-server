{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Types.Authors where

import Types.Infrastructure
import Control.Lens.TH
import Types.Users          (HasUserId)

import qualified Types.Users as Users

newTextType "Description"
newUUIDType "ID"

data Payload = Payload { payloadUserId      :: ID
                       , payloadDescription :: Description
                       }
deriveWeb "payload" ''Payload
makeFields ''Payload

newtype UpdateForm = UpdateForm { updateFormDescription :: Description }
deriveWeb "updateForm" ''UpdateForm
makeFields ''UpdateForm
