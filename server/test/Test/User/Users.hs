{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.User.Users (spec_users) where

import Universum hiding (get)
import Test.Tasty.Hspec

import Effects.Users
import Test.MonadStack

spec_users :: Spec
spec_users = describe "Users API" do
  spec_get

spec_get :: Spec
spec_get = describe "get method" do
    it "have to get the best test user payload" do
      runPure get `shouldBe` Right userPayload
