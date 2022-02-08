{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Admin.Authors (spec_authors) where

import Universum hiding (get)
import Test.Tasty.Hspec

import Effects.Authors
import Types.Router
import Test.MonadStack


spec_authors :: Spec
spec_authors = describe "Authors API" do
  spec_downgrade
  spec_update
  spec_get

spec_get :: Spec
spec_get = describe "get method" do
    it "have to get the best test author payload" do
      runPure (get authorId) `shouldBe` Right (Just authorPayload)

spec_downgrade :: Spec
spec_downgrade = describe "downgrade method" do
    it "have to always do it successfully" do
      runPure (downgrade authorId) `shouldBe` Right NoContent
      
spec_update :: Spec
spec_update = describe "update method" do
    it "have to always do it successfully and return payload with its' arguments" do
      runPure (update authorId authorDesc) `shouldBe` Right authorPayload
