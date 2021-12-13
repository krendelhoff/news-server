{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types.Common where

import Universum
import Types.TH

newUTCTimeType "CurrentTime"
