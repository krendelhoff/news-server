{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Pictures where

import Universum

import Types.TH (deriveWeb, newUUIDType)

newUUIDType "ID"

newtype Payload = Payload { payloadPictureId :: ID }
deriveWeb "payload" ''Payload
