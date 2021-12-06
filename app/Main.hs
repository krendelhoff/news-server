{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main where

import Hasql.Connection
import Network.HTTP.Types.Status
import Network.Wai
import Universum

import qualified Network.Wai.Handler.Warp as Warp

import API
import DB        (withConn)
import Migration (applyMigrations)
import Types.DB (ConnectionString)

settings :: Warp.Settings
settings = Warp.setBeforeMainLoop (putStrLn @Text "Server started...")
             Warp.defaultSettings

-- parse from dhall
-- settings function
connStr :: ConnectionString
connStr = "host=localhost port=5435 user=savely dbname=db password="

main :: IO ()
main = withConn connStr applyMigrations
