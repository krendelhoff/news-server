{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Types.Pictures where

import Universum

import Types.Common

newUUIDType "ID"

newtype Payload = Payload { payloadPictureId :: ID }
deriveWeb "payload" ''Payload
