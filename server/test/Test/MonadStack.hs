{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
module Test.MonadStack where

import Universum hiding (get, fromString)
import Control.Monad.Except

import Data.UUID (nil, fromString)

import Types.TH
import Errors

newtype PureMonad a = PureMonad { runPureMonad :: Except ServerError a }
  deriving newtype (Functor, Applicative, Monad, MonadError ServerError)

runPure :: PureMonad a -> Either ServerError a
runPure = runExcept . runPureMonad

runPureReader :: env -> ReaderT env PureMonad a -> Either ServerError a
runPureReader h m = runPure $ runReaderT m h

toID :: IsUUID a => String -> a
toID = fromUUID . fromMaybe nil . fromString

stub :: a
stub = error "not relevant"
