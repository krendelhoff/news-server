{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Utils where

import Universum

import Types.Infrastructure
import Data.Aeson.TH
import Data.Aeson

newUTCTimeType "CurrentTime"

data SumType a b = SumA a | SumB b deriving (Eq, Show)
deriveJSON defaultOptions { sumEncoding = UntaggedValue } ''SumType
