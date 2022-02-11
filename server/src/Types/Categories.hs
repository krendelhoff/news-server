{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Types.Categories where

import Universum
import Control.Lens.TH

import Types.Infrastructure

newUUIDType "ID"
newTextType "Title"

data Payload = Payload { payloadCategoryId :: ID
                       , payloadTitle      :: Title
                       , payloadParent     :: Maybe ID
                       }
deriveWeb "payload" ''Payload
makeFields ''Payload

data PayloadRecursive = PayloadRecursive
  { payloadRecursiveCategoryId :: ID
  , payloadRecursiveTitle :: Title
  , payloadRecursiveParent :: Maybe PayloadRecursive
  }
deriveWeb "payloadRecursive" ''PayloadRecursive

data CreateForm = CreateForm { createFormTitle :: Title
                             , createFormParent :: Maybe ID
                             }
deriveWeb "createForm" ''CreateForm
