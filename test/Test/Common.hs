{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Common where

import Test.Tasty.Hspec
import Universum

import Common
import Types.Router

spec_extractToken :: Spec
spec_extractToken = do
  let token = "f859245f78addaae96b5d12a736684bf42e924642132d924b66b1d72a010a0fb"
      case1 = [("Authorization", "very very bad token")]
      case2 = [("Authoron", "doesn't matter!")]
      case3 = [("Authorization", "Bearer " <> fromString token)]
      case4 = [("Authorization", fromString token)]
  describe "extractToken (from request header) util" do
    describe "have to extract valid token or fail gracefully" do
      context "when token violates format" do
        it "returns BadToken error" do
          extractToken case1 `shouldBe` Left BadToken
          extractToken case4 `shouldBe` Left BadToken
      context "when there is no Authorization header" do
        it "returns NoToken" do
          extractToken case2 `shouldBe` Left NoToken
      context "when there is Authorization header and token has correct format" do
        it "returns Token value" do
          extractToken case3 `shouldBe` Right (fromString token)
