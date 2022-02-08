{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.User.Pictures (spec_pictures) where

import Universum hiding (get)
import Test.Tasty.Hspec

import Effects.Pictures
import Test.MonadStack

spec_pictures :: Spec
spec_pictures = describe "Pictures API" do
  spec_get

spec_get :: Spec
spec_get = describe "get method" do
    it "have to return generated UUID of the uploaded picture" do
      runPure (persist "*big floppa picture*") `shouldBe` Right pictureId
