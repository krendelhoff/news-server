{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.User.Categories (spec) where

import Universum hiding (Handle, get)
import Test.Tasty.Hspec

import Test.Effects.Categories
import Types.Utils
import Server.Errors
import Test.MonadStack
import Server.User.Categories

spec :: Spec
spec = describe "Categories User API" do
  spec_get

case1 :: Handle PureMonad
case1 = Handle { tget          = const (return Nothing)
               , tgetRecursive = stub
               , tcreate       = stub
               , tremove       = stub
               , trename       = stub
               , trebase       = stub
               , trootID       = stub
               }

case2 :: Handle PureMonad
case2 = Handle { tget          = const (return $ Just payload)
               , tgetRecursive = stub
               , tcreate       = stub
               , tremove       = stub
               , trename       = stub
               , trebase       = stub
               , trootID       = stub
               }

case3 :: Handle PureMonad
case3 = Handle { tget          = stub
               , tgetRecursive = const (return Nothing)
               , tcreate       = stub
               , tremove       = stub
               , trename       = stub
               , trebase       = stub
               , trootID       = stub
               }

case4 :: Handle PureMonad
case4 = Handle { tget          = stub
               , tgetRecursive = const (return $ Just recursivePayload)
               , tcreate       = stub
               , tremove       = stub
               , trename       = stub
               , trebase       = stub
               , trootID       = stub
               }

spec_get :: Spec
spec_get = describe "get method" do
  describe "have to get test category payload" do
    context "when requested category does not exist" do
      it "returns categoryNotFoundError" do
        runPureReader case1 (get Nothing testId) `shouldBe` Left categoryNotFoundError
    context "otherwise" do
      it "returns category payload" do
        runPureReader case2 (get (Just False) testId) `shouldBe` Right (SumA payload)
  describe "have to get test category recursive payload" do
    context "when requested category does not exist" do
      it "returns categoryNotFoundError" do
        runPureReader case3 (get (Just True) testId) `shouldBe` Left categoryNotFoundError
    context "otherwise" do
      it "returns category payload" do
        runPureReader case4 (get (Just True) testId) `shouldBe` Right (SumB recursivePayload)
