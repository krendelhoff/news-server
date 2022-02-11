{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Admin.Users (spec) where


import Universum hiding (get, Handle)
import Test.Tasty.Hspec

import Test.MonadStack

import Server.Admin.Users
import Types.Router
import Server.Errors

import Test.Effects.Users

spec :: Spec
spec = describe "Users Admin API" do
  spec_delete

case1 :: Handle PureMonad
case1 = Handle { tget     = stub
               , tgetById = const (return Nothing)
               , tdelete  = stub
               }

case2 :: Handle PureMonad
case2 = Handle { tget     = stub
               , tgetById = const (return $ Just adminPayload)
               , tdelete  = stub
               }

case3 :: Handle PureMonad
case3 = Handle { tget     = stub
               , tgetById = const (return $ Just payload)
               , tdelete  = const pass
               }

spec_delete :: Spec
spec_delete = describe "delete method" do
  describe "have to delete specified user" do
    context "when requested user does not exist" do
      it "throws userNotFoundError" do
        runPureReader case1 (delete testId) `shouldBe` Left userNotFoundError
    context "when admin tries to delete another admin" do
      it "throws cantNotFoundError" do
        runPureReader case2 (delete testId) `shouldBe` Left cantRemoveAdminError
    context "when user exists and it is regular user" do
      it "deletes successfully" do
        runPureReader case3 (delete testId) `shouldBe` Right NoContent
