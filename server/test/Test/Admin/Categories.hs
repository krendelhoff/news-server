{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE OverloadedStrings    #-}
module Test.Admin.Categories (spec) where

import Universum hiding (Handle, get)
import Test.Tasty.Hspec

import Server.Admin.Categories
import Server.Errors
import Types.Router
import Test.MonadStack
import Test.Effects.Categories

spec :: Spec
spec = describe "Categories Admin API" do
  spec_create
  spec_remove
  spec_update

case1 :: Handle PureMonad
case1 = Handle { tget          = stub
               , tgetRecursive = stub
               , tcreate       = const (const (return Nothing))
               , tremove       = stub
               , trename       = stub
               , trebase       = stub
               , trootID       = stub
               }

case2 :: Handle PureMonad
case2 = Handle { tget          = stub
               , tgetRecursive = stub
               , tcreate       = const (const (return $ Just payload))
               , tremove       = stub
               , trename       = stub
               , trebase       = stub
               , trootID       = stub
               }

spec_create :: Spec
spec_create = describe "create method" do
  describe "have to create category" do
    context "when requested title is not unique" do
      it "throws titleIsNotUniqueError" do
        runPureReader case1 (create createForm) `shouldBe` Left titleIsNotUnique
    context "when create form correct" do
      it "returns created category data" do
        runPureReader case2 (create createForm) `shouldBe` Right payload

case3 :: Handle PureMonad
case3 = Handle { tget          = const (return Nothing)
               , tgetRecursive = stub
               , tcreate       = stub
               , tremove       = stub
               , trename       = stub
               , trebase       = stub
               , trootID       = return root
               }

case4 :: Handle PureMonad
case4 = Handle { tget          = stub
               , tgetRecursive = stub
               , tcreate       = stub
               , tremove       = stub
               , trename       = stub
               , trebase       = stub
               , trootID       = return root
               }

case5 :: Handle PureMonad
case5 = Handle { tget          = const (return $ Just payload)
               , tgetRecursive = stub
               , tcreate       = stub
               , tremove       = const pass
               , trename       = stub
               , trebase       = stub
               , trootID       = return root
               }

spec_remove :: Spec
spec_remove = describe "remove method" do
  describe "have to remove the category" do
    context "when requested category does not exist" do
      it "throws categoryNotFoundError" do
        runPureReader case3 (remove testId) `shouldBe` Left categoryNotFoundError
    context "when tries to remove root category" do
      it "throws cantRemoveRootError" do
        runPureReader case4 (remove root) `shouldBe` Left cantRemoveRootError
    context "otherwise" do
      it "removes successfully" do
        runPureReader case5 (remove testId) `shouldBe` Right NoContent

case6 :: Handle PureMonad
case6 = Handle { tget          = const (return Nothing)
               , tgetRecursive = stub
               , tcreate       = stub
               , tremove       = stub
               , trename       = stub
               , trebase       = stub
               , trootID       = stub
               }

case7 :: Handle PureMonad
case7 = Handle { tget          = const (return $ Just payload)
               , tgetRecursive = stub
               , tcreate       = stub
               , tremove       = stub
               , trename       = stub
               , trebase       = stub
               , trootID       = stub
               }

case8 :: Handle PureMonad
case8 = Handle
  { tget          = \x -> if x == testId then pure (Just payload)
                          else pure Nothing
  , tgetRecursive = stub
  , tcreate       = stub
  , tremove       = stub
  , trename       = stub
  , trebase       = stub
  , trootID       = stub
  }

case9 :: Handle PureMonad
case9 = Handle
  { tget          = \x -> if x == testId then pure (Just payload)
                          else pure (Just parentPayload)
  , tgetRecursive = stub
  , tcreate       = stub
  , tremove       = stub
  , trename       = stub
  , trebase       = const (const (pure Nothing))
  , trootID       = stub
  }

case10 :: Handle PureMonad
case10 = Handle
  { tget          = const $ pure (Just payload)
  , tgetRecursive = stub
  , tcreate       = stub
  , tremove       = stub
  , trename       = const (const $ pure Nothing)
  , trebase       = stub
  , trootID       = stub
  }

case11 :: Handle PureMonad
case11 = Handle
  { tget          = const $ pure (Just payload)
  , tgetRecursive = stub
  , tcreate       = stub
  , tremove       = stub
  , trename       = const (const $ pure (Just payload))
  , trebase       = const (const $ pure (Just payload))
  , trootID       = stub
  }

spec_update :: Spec
spec_update = describe "update method" do
  describe "update category title or change the parent" do
    context "when requested category does not exist" do
      it "throws categoryNotFoundError" do
        runPureReader case6 (update testId Nothing Nothing)
          `shouldBe` Left categoryNotFoundError
    context "when nothing to update" do
      it "returns the same payload" do
        runPureReader case7 (update testId Nothing Nothing)
          `shouldBe` Right payload
    context "when rebase destination does not exist" do
      it "returns rebaseDestinationNotExist" do
        runPureReader case8 (update testId Nothing (Just testParentId))
          `shouldBe` Left rebaseDestinationNotExist
    context "when rebase destination incorrect (forms a cycle)" do
      it "returns incorrectRebaseDestination" do
        runPureReader case9 (update testId Nothing (Just testParentId))
          `shouldBe` Left incorrectRebaseDestination
    context "when new name is not unique" do
      it "returns titleIsNotUnique" do
        runPureReader case10 (update testId (Just title) Nothing)
          `shouldBe` Left titleIsNotUnique
    context "when input data is correct" do
      it "successfully updates category and returns payload" do
        runPureReader case11 (update testId (Just title) (Just testParentId))
          `shouldBe` Right payload
