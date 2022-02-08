{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.User.Categories (spec_user_categories) where

import Universum hiding (get)
import Test.Tasty.Hspec

import Effects.Categories
import Types.Router
import Server.Errors
import Test.MonadStack
import Server.Admin.Categories (update)

spec_user_categories :: Spec
spec_user_categories = describe "Categories User API" do
  spec_get
  spec_getRecursive

spec_get :: Spec
spec_get = describe "get method" do
    it "have to get the best test category payload" do
      runPure (get categoryId) `shouldBe` Right (Just categoryPayload)

spec_getRecursive :: Spec
spec_getRecursive = describe "getRecursive method" do
    it "have to get the best test category recursive payload" do
      runPure (getRecursive categoryId) `shouldBe` Right (Just recCategoryPayload)

