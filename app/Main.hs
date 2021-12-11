{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main where

import Dhall
import Hasql.Connection          hiding (settings)
import Network.HTTP.Types.Status
import Network.Wai
import Universum                 hiding (toText)

import qualified Hasql.Pool               as Pool
import qualified Network.Wai.Handler.Warp as Warp

import DB
import Migration         (applyMigrations)
import Router
import Types.DB
import Types.Environment
import Types.TH
import Data.Aeson

newTextType "Title"

newEnumType "EnumType" ["EnumTypeOne", "EnumTypeTwo", "EnumTypeThree"]

serverSettings :: Warp.Settings
serverSettings = Warp.setBeforeMainLoop
                   (putStrLn @Text "Server initialized at localhost:5435...")
                   Warp.defaultSettings

type MyAPI = "boba" :> "biba" :> Get Int

app :: Server MyAPI
app = return 5

main :: IO ()
main = do -- TODO Cont
  conf <- readFile "config.dhall" >>= input dbConfigDecoder
  let poolSettings = (10, 5, mkConnStr conf)
  withPool poolSettings \pool -> do
    applyMigrations pool
    putStrLn $ Universum.maybe "" encode (decode "\"one\"" :: Maybe EnumType)
    let ?env = Environment pool conf
     in Warp.runSettings serverSettings $ serve @MyAPI app
