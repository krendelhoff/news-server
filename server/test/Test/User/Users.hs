{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.User.Users (spec) where

import Universum hiding (get, Handle)
import Test.Tasty.Hspec

import Test.MonadStack

import Server.User.Users

import Test.Effects.Users

spec :: Spec
spec = describe "Users User API" do
  spec_get

case1 :: Handle PureMonad
case1 = Handle { tget     = return payload
               , tgetById = stub
               , tdelete  = stub
               }

spec_get :: Spec
spec_get = describe "get method" do
  it "have to get the authenticated user payload" do
    runPureReader case1 get `shouldBe` Right payload
