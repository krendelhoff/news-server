{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Lenses ( Password
                    , HasPassword(..)
                    , Name
                    , HasName(..)
                    , Token
                    , HasToken(..)
                    , HasPool(..)
                    ) where

import Control.Lens
import Hasql.Pool   (Pool)
import Universum

import Types.TH

newTextType "Token"
newTextType "Name"
newTextType "Password"

newtype DummyPassword = DummyPassword { _password :: Password }
makeFieldsNoPrefix ''DummyPassword

newtype DummyName = DummyName { _name :: Name }
makeFieldsNoPrefix ''DummyName

newtype DummyToken = DummyToken { _token :: Token }
makeFieldsNoPrefix ''DummyToken

newtype DummyPool = DummyPool { _pool :: Pool }
makeFieldsNoPrefix ''DummyPool
