{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.DB where

import Hasql.Connection
import Hasql.Migration
import Universum

type ConnectionString = ByteString

instance Exception ConnectionError
instance Exception MigrationError
