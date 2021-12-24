{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Common ( module Types.TH
                    , module Types.TH.Classes
                    , module Types.Lenses
                    , module Types.Errors
                    , module Types.DB
                    , StdMethod(..)
                    , CurrentTime
                    ) where

import Universum

import Types.DB
import Network.HTTP.Types
import Types.Errors
import Types.Lenses
import Types.TH
import Types.TH.Classes

newUTCTimeType "CurrentTime"

-- FIXME normal module structure
-- TODO dependency graph
