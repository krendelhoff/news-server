{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Common where

import Types.TH
import Universum

newUTCTimeType "CurrentTime"
