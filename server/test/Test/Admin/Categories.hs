{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Test.Admin.Categories (spec_admin_categories) where

import Universum hiding (get)
import Test.Tasty.Hspec

import Effects.Categories
import Types.Router
import Server.Errors
import Test.MonadStack
import Server.Admin.Categories (update)

spec_admin_categories :: Spec
spec_admin_categories = describe "Categories Admin API" do
  spec_create
  spec_remove
  spec_rename
  spec_rebase
  spec_update


spec_create :: Spec
spec_create = describe "create method" do
    it "have to create the best test category payload" do
      runPure (create categoryTitle (Just categoryParent)) `shouldBe` Right (Just categoryPayload)

spec_remove :: Spec
spec_remove = describe "remove method" do
    it "have to remove the best test category" do
      runPure (remove categoryId) `shouldBe` Right NoContent

spec_rename :: Spec
spec_rename = describe "rename method" do
    it "have to rename the best test category" do
      runPure (rename categoryId categoryTitle) `shouldBe` Right (Just categoryPayload)

spec_rebase :: Spec
spec_rebase = describe "rebase method" do
    it "have to rebase the best test category" do
      runPure (rebase categoryId categoryParent) `shouldBe` Right (Just categoryPayload)

handleForCategoryNotFoundError = TestHandle
  { tget = const (pure Nothing)
  , tgetRecursive = undefined
  , tcreate = undefined
  , tremove = undefined
  , trename = undefined
  , trebase = undefined
  }
handleForRebaseDestinationNotExist = TestHandle
  { tget = \x -> if x == categoryId then pure (Just categoryPayload) else pure Nothing
  , tgetRecursive = error "tgetRec"
  , tcreate = error "tcreate"
  , tremove = error "tremove"
  , trename = error "trename"
  , trebase = error "trebase"
  }
handleForIncorrentRebaseDestination = TestHandle
  { tget = \x -> if x == categoryId then pure (Just categoryPayload) else pure (Just parentCategoryPayload)
  , tgetRecursive = undefined
  , tcreate = undefined
  , tremove = undefined
  , trename = undefined
  , trebase = const (const (pure Nothing))
  }
handleForTitleIsNotUnique = TestHandle
  { tget = const $ pure (Just categoryPayload)
  , tgetRecursive = undefined
  , tcreate = undefined
  , tremove = undefined
  , trename = const (const $ pure Nothing)
  , trebase = undefined
  }
handleForEverythingCorrect = TestHandle
  { tget = const $ pure (Just categoryPayload)
  , tgetRecursive = undefined
  , tcreate = undefined
  , tremove = undefined
  , trename = const (const $ pure (Just categoryPayload))
  , trebase = const (const $ pure (Just categoryPayload))
  }

spec_update :: Spec
spec_update = describe "update method" do
  describe "have to always do it successfully or return various kinds of errors" do
    context "when there isn't category with given ID" do
      it "returns categoryNotFoundError" do
        runPureReader handleForCategoryNotFoundError (update categoryId Nothing Nothing) `shouldBe` Left categoryNotFoundError
    context "when nothing to update" do
      it "returns the same payload" do
        runPure (update categoryId Nothing Nothing) `shouldBe` Right categoryPayload
    context "when rebase destination does not exist" do
      it "returns rebaseDestinationNotExist" do
        runPureReader handleForRebaseDestinationNotExist (update categoryId Nothing (Just categoryParent)) `shouldBe` Left rebaseDestinationNotExist
    context "when rebase destination incorrent (forms a cycle)" do
      it "returns incorrectRebaseDestination" do
        runPureReader handleForIncorrentRebaseDestination (update categoryId Nothing (Just categoryParent)) `shouldBe` Left incorrectRebaseDestination
    context "when new name is not unique" do
      it "returns titleIsNotUnique" do
        runPureReader handleForTitleIsNotUnique (update categoryId (Just categoryTitle) Nothing) `shouldBe` Left titleIsNotUnique
    context "when input data is correct" do
      it "successfully updates category and returns payload" do
        runPureReader handleForEverythingCorrect (update categoryId (Just categoryTitle) (Just parentCategoryId)) `shouldBe` Right categoryPayload
