{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Pictures where

import Universum

import Types.Infrastructure

newUUIDType "ID"

newtype Payload = Payload { payloadPictureId :: ID }
deriveWeb "payload" ''Payload
