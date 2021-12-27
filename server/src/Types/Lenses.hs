{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
module Types.Lenses where

import Control.Lens (Iso', iso, makeFieldsNoPrefix)
import Data.Time    (UTCTime)
import Hasql.Pool   (Pool)
import Universum    hiding (toText)

import Types.TH (IsText(..), IsUTCTime(..))

newtype DummyPool = DummyPool { _pool :: Pool }
makeFieldsNoPrefix ''DummyPool

instance HasPool Pool Pool where
  pool = id

utctime :: IsUTCTime a => Iso' a UTCTime
utctime = iso toUTCTime fromUTCTime

text :: IsText a => Iso' a Text
text = iso toText fromText
