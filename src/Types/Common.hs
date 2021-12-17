{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Common ( module Types.TH
                    , module Types.Lenses
                    , module Types.Errors
                    , CurrentTime
                    ) where

import Types.TH
import Types.Errors
import Types.Lenses
import Universum

newUTCTimeType "CurrentTime"
