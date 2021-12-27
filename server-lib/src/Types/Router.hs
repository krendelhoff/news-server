{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Types.Router where

import Control.Lens         (makeLenses)
import Control.Monad.Except
import Data.Aeson           (ToJSON(toJSON), object)
import GHC.TypeLits         (Nat, Symbol)
import Network.HTTP.Types   (Query, RequestHeaders, StdMethod(..))
import Universum

import qualified Data.ByteString.Lazy as BL

import Errors           (ServerError)
import Types.TH.Classes (makeKnown, makeKnown', stripModifier)

newtype Handler a = Handler
  { runHandler :: ReaderT (Maybe ByteString) (ExceptT ServerError IO) a }
  deriving newtype ( Functor, Applicative, Monad, MonadError ServerError
                   , MonadThrow, MonadCatch, MonadMask, MonadIO
                   , MonadReader (Maybe ByteString)
                   )

data RoutingInfo = RoutingInfo { _path     :: [Text]
                               , _method   :: ByteString
                               , _queryStr :: Query
                               , _headers  :: RequestHeaders
                               , _body     :: BL.ByteString
                               , _state    :: Maybe ServerError
                               }
makeLenses ''RoutingInfo

data SMethod (m :: StdMethod) where
  SPut    :: SMethod 'PUT
  SPost   :: SMethod 'POST
  SGet    :: SMethod 'GET
  SDelete :: SMethod 'DELETE

makeKnown' (stripModifier "Std") ''StdMethod

data Verb (m :: StdMethod) (code :: Nat) (f :: Format) (a :: Type)

type Get a    = Verb 'GET 200 'JSON a
type Put a    = Verb 'PUT 200 'JSON a
type Post a   = Verb 'POST 200 'JSON a
type Delete a = Verb 'DELETE 200 'JSON a

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: Type)
infixr 9 :>

data Capture (id :: Symbol) (a :: Type)

data QueryParam (s :: Symbol) (a :: Type)

data QueryParams (s :: Symbol) (a :: Type)

data Format = Raw | JSON

makeKnown ''Format

data ReqBody (f :: Format) (a :: Type)

data NoContent = NoContent

instance ToJSON NoContent where
  toJSON _ = object []
