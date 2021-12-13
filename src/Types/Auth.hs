{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
module Types.Auth where

import Universum

import Types.TH

newBoolType "IsAdmin"
newTextType "Token"
newBoolType "IsExpired"
newUTCTimeType "CurrentTime"

data TokenInfo = TokenInfo { tokenInfoToken   :: Token
                           , tokenInfoAdmin   :: IsAdmin
                           , tokenInfoExpired :: IsExpired
                           } deriving (Eq, Show)
