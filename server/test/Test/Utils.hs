{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Utils where

import Test.Tasty.Hspec
import Universum

import Utils

spec_utils :: Spec
spec_utils = spec_extractToken >> spec_combinator

spec_combinator :: Spec
spec_combinator = do
  describe "monadic combinator ==>" do
    describe "have to have expected semantics" do
      it "passes the test" do
        (Identity 5 =>> const (Identity 6)) `shouldBe` Identity 5

spec_extractToken :: Spec
spec_extractToken = do
  let token = "f859245f78addaae96b5d12a736684bf42e924642132d924b66b1d72a010a0fb"
      case1 = "very very bad token"
      case2 = "doesn't matter!"
      case3 = "Bearer " <> fromString token
      case4 = fromString token
  describe "extractToken (from request header) util" do
    describe "have to extract valid token or fail gracefully" do
      context "when token violates format" do
        it "fail to parse" do
          parseToken case1 `shouldBe` Nothing
          parseToken case2 `shouldBe` Nothing
          parseToken case4 `shouldBe` Nothing
      context "when there is Authorization header and token has correct format" do
        it "returns Token value" do
          parseToken case3 `shouldBe` Just (fromString token)
