{-# LANGUAGE DerivingVia #-}
module Types.Router where

import Universum
import Control.Monad.Except

import Types.Environment

data ServerError = U | E deriving (Eq, Show)

instance Semigroup ServerError where
  U <> U = U
  _ <> _ = E

instance Monoid ServerError where
  mempty = U

newtype Handler a = Handler
  { runHandler :: ReaderT Environment (ExceptT ServerError IO) a }
  deriving ( Functor, Applicative, Monad
           , MonadError ServerError, Alternative
           , MonadReader Environment )
  via (ReaderT Environment (ExceptT ServerError IO))

type Request = [Text]
