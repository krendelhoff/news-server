{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Categories where

import Universum

import Types.Infrastructure

newUUIDType "ID"
newTextType "Title"

data Payload = Payload { payloadCategoryId :: ID
                       , payloadTitle      :: Title
                       , payloadParent     :: Maybe ID
                       }
deriveWeb "payload" ''Payload

data PayloadRecursive = PayloadRecursive
  { payloadRecursiveCategoryId :: ID
  , payloadRecursiveTitle :: Title
  , payloadRecursiveParent :: Maybe PayloadRecursive
  }
deriveWeb "payloadRecursive" ''PayloadRecursive
