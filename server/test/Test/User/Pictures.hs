{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.User.Pictures (spec) where

import Universum hiding (Handle, get)
import Test.Tasty.Hspec

import Test.MonadStack
import Test.Effects.Pictures
import Server.User.Pictures

spec :: Spec
spec = describe "Pictures API" do
  spec_get

case1 :: Handle PureMonad
case1 = Handle { tpersist = const (return testId) }

spec_get :: Spec
spec_get = describe "get method" do
  it "Have to return generated UUID of the uploaded picture" do
    runPureReader case1 (persist "*big floppa picture*") `shouldBe` Right payload
