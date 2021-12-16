{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Common ( module Types.TH
                    , module Types.Lenses
                    , CurrentTime
                    ) where

import Types.TH
import Types.Lenses
import Universum

newUTCTimeType "CurrentTime"
