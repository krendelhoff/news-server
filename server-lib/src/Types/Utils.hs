{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Utils where

import Universum

import Types.TH (newUTCTimeType)

newUTCTimeType "CurrentTime"
