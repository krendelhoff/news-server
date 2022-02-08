{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Database.Users where
{-
import Test.Tasty.Hspec
import Data.Coerce (coerce)
import Universum        hiding (get)

import Common         hiding (run)
import Database.Users
import Test.Database
import Types.Users

spec_create :: Spec
spec_create = beforeAll_ setupDatabase $
  around (withConn testDbConnStr) do
    describe "user creation" do
      context "empty database" do
        it "successfully creates user" \conn -> do
          let passwordHash = hash "test123"
          run conn (create "Savely" "Krendelhoff" "krendelhoff"
                      Nothing passwordHash ) >>= either (fail . show) \uid -> do
            curTime <- getCurrentTime

            run conn (get uid) >>= either (fail . show) \result -> do
              let payload = Payload { payloadId = uid
                                    , payloadName = "Savely"
                                    , payloadSurname = "Krendelhoff"
                                    , payloadLogin = "krendelhoff"
                                    , payloadAvatar = Nothing
                                    , payloadCreatedAt = result^.createdAt
                                    , payloadPrivileged = coerce False
                                    }
              result `shouldBe` payload

-}
