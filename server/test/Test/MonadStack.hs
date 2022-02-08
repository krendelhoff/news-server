{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
module Test.MonadStack where

import Universum hiding (get, fromString)
import Control.Monad.Except
import Data.UUID (nil, fromString)
import Data.Time.Clock.POSIX
import Data.Maybe (fromMaybe)

import Types.TH
import Infrastructure
import Errors

import qualified Effects.Authors    as Authors
import qualified Types.Authors      as Authors
import qualified Effects.Pictures   as Pictures
import qualified Types.Pictures     as Pictures
import qualified Effects.Users      as Users
import qualified Types.Users        as Users
import qualified Effects.Categories as Categories
import qualified Types.Categories   as Categories


newtype PureMonad a = PureMonad { runPureMonad :: Except ServerError a }
  deriving newtype (Functor, Applicative, Monad, MonadError ServerError)

runPure :: PureMonad a -> Either ServerError a
runPure = runExcept . runPureMonad


instance Categories.AcquireCategory PureMonad where
  get _          = pure $ Just categoryPayload
  getRecursive _ = pure $ Just recCategoryPayload

instance Categories.PersistCategory PureMonad where
  create _ _ = pure $ Just categoryPayload
  remove _   = pure NoContent
  rename _ _ = pure $ Just categoryPayload
  rebase _ _ = pure $ Just categoryPayload

data TestHandle m = TestHandle
  { tget          :: Categories.ID -> m (Maybe Categories.Payload)
  , tgetRecursive :: Categories.ID -> m (Maybe Categories.PayloadRecursive)
  , tcreate       :: Categories.Title -> Maybe Categories.ID -> m (Maybe Categories.Payload)
  , tremove       :: Categories.ID -> m NoContent
  , trename       :: Categories.ID -> Categories.Title -> m (Maybe Categories.Payload)
  , trebase       :: Categories.ID -> Categories.ID -> m (Maybe Categories.Payload)
  }

instance Categories.AcquireCategory (ReaderT (TestHandle PureMonad) PureMonad) where
  get x = do
    handle <- ask
    lift $ tget handle x
  getRecursive x = do
    handle <- ask
    lift $ tgetRecursive handle x

instance Categories.PersistCategory (ReaderT (TestHandle PureMonad) PureMonad) where
  create x y = do
    handle <- ask
    lift $ tcreate handle x y
  remove _   = pure NoContent
  rename x y = do
    handle <- ask
    lift $ trename handle x y
  rebase x y = do
    handle <- ask
    lift $ trebase handle x y

runPureReader :: TestHandle PureMonad -> ReaderT (TestHandle PureMonad) PureMonad a -> Either ServerError a
runPureReader h m = runPure $ runReaderT m h

categoryId = fromUUID nil
categoryTitle = "Best category"
categoryParent = parentCategoryId
parentCategoryPayload = Categories.Payload parentCategoryId "Parent of the best category" Nothing
parentCategoryId = fromUUID $ fromMaybe nil $ fromString "d2cb024c-98d1-4b99-a56b-7416c1d59484"
categoryPayload = Categories.Payload categoryId categoryTitle (Just categoryParent)
recCategoryPayload = Categories.PayloadRecursive categoryId "Best category"
 (Just $ Categories.PayloadRecursive parentCategoryId "Parent of the best category" Nothing)

-- FIXME actually i wanna to see in tests that
-- if I do update then get function will get me updated version
-- need to do something like in-memory db
instance Authors.AcquireAuthor PureMonad where
  get _ = pure $ Just authorPayload
  
authorId = fromUUID nil
authorDesc = "The best test author"
authorPayload = Authors.Payload authorId authorDesc

instance Authors.PersistAuthor PureMonad where
  downgrade _ = pure NoContent
  update = (pure .) . Authors.Payload

bestUserId = fromUUID nil
creationTime = fromUTCTime $ posixSecondsToUTCTime 0
userPayload = Users.Payload bestUserId "Savely" "Krendelhoff"
                            "krendelhoff" Nothing
                            creationTime (fromBool True)
                            
instance Users.AcquireUser PureMonad where
  get = pure userPayload

pictureId = fromUUID nil
instance Pictures.PersistPicture PureMonad where
  persist _ = pure pictureId
