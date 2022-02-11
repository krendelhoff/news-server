{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Admin.Authors (spec) where

import Universum hiding (get, Handle)
import Test.Tasty.Hspec

import Server.Admin.Authors
import Server.Errors
import Types.Router
import Test.Effects.Authors
import Test.MonadStack

spec :: Spec
spec = describe "Authors Admin API" do
  spec_promote
  spec_downgrade
  spec_update
  spec_get

case1 :: Handle PureMonad
case1 = Handle
  { tget       = const (return Nothing)
  , tdowngrade = stub
  , tupdate    = stub
  , tpromote   = const (const (return Nothing))
  , tusers     = stub
  }

case2 :: Handle PureMonad
case2 = Handle
  { tget       = const (return $ Just payload)
  , tdowngrade = stub
  , tupdate    = stub
  , tpromote   = stub
  , tusers     = stub
  }

case3 :: Handle PureMonad
case3 = Handle
  { tget       = const (return Nothing)
  , tdowngrade = stub
  , tupdate    = stub
  , tpromote   = const (const (return $ Just payload))
  , tusers     = stub
  }

spec_promote :: Spec
spec_promote = describe "promote method" do
  describe "have to promote regular user to author" do
    context "when requested user does not exist" do
      it "throws userNotFoundError" do
        runPureReader case1 (promote payload)
          `shouldBe` Left userNotFoundError
    context "when user is already an author" do
      it "throws authorAlreadyExistsError" do
        runPureReader case2 (promote payload)
          `shouldBe` Left authorAlreadyExistsError
    context "otherwise" do
      it "promotes successfully" do
        runPureReader case3 (promote payload)
          `shouldBe` Right payload

case4 :: Handle PureMonad
case4 = Handle
  { tget       = const (return Nothing)
  , tdowngrade = stub
  , tupdate    = stub
  , tpromote   = stub
  , tusers     = stub
  }

case5 :: Handle PureMonad
case5 = Handle
  { tget       = const (return $ Just payload)
  , tdowngrade = const pass
  , tupdate    = stub
  , tpromote   = stub
  , tusers     = stub
  }

spec_downgrade :: Spec
spec_downgrade = describe "downgrade method" do
  describe "have to downgrade author to regular user" do
    context "when requested author does not exists" do
      it "throws authorNotFoundError" do
        runPureReader case4 (downgrade testId)
          `shouldBe` Left authorNotFoundError
    context "otherwise" do
      it "downgrades author successfully" do
        runPureReader case5 (downgrade testId)
          `shouldBe` Right NoContent

case6 :: Handle PureMonad
case6 = Handle
  { tget       = stub
  , tdowngrade = stub
  , tupdate    = const (const (return Nothing))
  , tpromote   = stub
  , tusers     = stub
  }

case7 :: Handle PureMonad
case7 = Handle
  { tget       = stub
  , tdowngrade = stub
  , tupdate    = const (const (return $ Just payload))
  , tpromote   = stub
  , tusers     = stub
  }


spec_update :: Spec
spec_update = describe "update method" do
  describe "have to update author's description" do
    context "requested author does not exists" do
      it "throws authorNotFoundError" do
        runPureReader case6 (update testId updateForm)
          `shouldBe` Left authorNotFoundError
    context "otherwise" do
      it "updates successfully" do
        runPureReader case7 (update testId updateForm)
          `shouldBe` Right payload

case8 :: Handle PureMonad
case8 = Handle
  { tget       = const (return Nothing)
  , tdowngrade = stub
  , tupdate    = stub
  , tpromote   = stub
  , tusers     = stub
  }

case9 :: Handle PureMonad
case9 = Handle
  { tget       = const (return $ Just payload)
  , tdowngrade = stub
  , tupdate    = stub
  , tpromote   = stub
  , tusers     = stub
  }

spec_get :: Spec
spec_get = describe "get method" do
  describe "have to get author's data" do
    context "requested author does not exists" do
      it "throws authorNotFoundError" do
        runPureReader case8 (get testId)
          `shouldBe` Left authorNotFoundError
    context "otherwise" do
      it "returns payload successfully" do
        runPureReader case9 (get testId)
          `shouldBe` Right payload
