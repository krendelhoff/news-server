{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
module Types.Auth where

import Universum

import Types.TH
import Types.Users

import qualified Types.Users as Users (ID)

newBoolType "IsExpired"

data TokenInfo = TokenInfo { tokenInfoToken   :: Token
                           , tokenInfoAdmin   :: IsAdmin
                           , tokenInfoExpired :: IsExpired
                           , tokenInfoId      :: Users.ID
                           } deriving (Eq, Show)
