{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Types.Router where

import Control.Lens         (makeLenses)
import Control.Monad.Except
import Data.Aeson           (ToJSON(toJSON), object, FromJSON)
import Data.UUID            (UUID)
import GHC.TypeLits         (Symbol)
import Network.HTTP.Types   (Query, RequestHeaders, StdMethod(..))
import Universum

import qualified Data.ByteString.Lazy as BL

import Errors           (ServerError)
import Types.TH.Classes (makeKnown, makeKnown', stripModifier)
import Web.HttpApiData (FromHttpApiData)

newtype Handler a = Handler
  { runHandler :: ExceptT ServerError IO a }
  deriving newtype ( Functor, Applicative, Monad, MonadError ServerError
                   , MonadThrow, MonadCatch, MonadMask, MonadIO
                   )

type Path = [Text]


data Protection = Regular | Protected deriving (Eq, Ord, Show)
makeKnown ''Protection

data Auth (protection :: Protection) (a :: Type)

data RawRoutePiece where
  RawQueryP :: FromHttpApiData a => String  -> Proxy a       -> ByteString    -> RawRoutePiece
  RawCapt   :: FromHttpApiData a => String  -> Proxy a       -> Text          -> RawRoutePiece
  RawReqB   :: FromJSON a        =>            Proxy a       -> BL.ByteString -> RawRoutePiece

data RoutePiece where
  QueryP :: FromHttpApiData a => String  -> a      -> RoutePiece
  Capt   :: FromHttpApiData a => String  -> a      -> RoutePiece
  ReqB   :: FromJSON a        =>            a      -> RoutePiece


data RawAuthData = RawAuthData
  { _userId       :: !UUID
  , _token        :: !Text
  , _refreshToken :: !Text
  , _isAdmin      :: !Bool
  , _authType     :: !AuthType
  } deriving (Eq, Show)

data AuthType = Access | Refresh deriving (Eq, Show, Generic, FromJSON, ToJSON)

data AuthResult a = NoToken | BadToken | NotFound | TokenExpired | AuthSuccess a
  deriving stock (Functor, Eq, Show)

--instance Functor AuthResult where
--  fmap f NotFound        = NotFound
--  fmap f TokenExpired    = TokenExpired
--  fmap f (AuthSuccess a) = AuthSuccess $ f a

data ServingHandle = ServingHandle
  { _extractToken  :: ByteString -> Maybe Text
  , _authenticate  :: Text       -> IO (AuthResult RawAuthData)
  , _log           :: Text       -> IO ()
  }

data RoutingEnv = RoutingEnv { _path     :: Path
                             , _method   :: StdMethod
                             , _queryStr :: Query
                             , _headers  :: RequestHeaders
                             , _body     :: BL.ByteString
                             , _handle   :: ServingHandle
                             , _auth     :: AuthResult RawAuthData
                             }
makeLenses ''RoutingEnv

data SMethod (m :: StdMethod) where
  SPut    :: SMethod 'PUT
  SPost   :: SMethod 'POST
  SGet    :: SMethod 'GET
  SDelete :: SMethod 'DELETE

makeKnown' (stripModifier "Std") ''StdMethod

data Verb (m :: StdMethod) (code :: Nat) (f :: Format) (a :: Type)

type Get a    = Verb 'GET    200 'JSON a
type Put a    = Verb 'PUT    200 'JSON a
type Post a   = Verb 'POST   200 'JSON a
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

data NoContent = NoContent deriving (Eq, Show)

instance ToJSON NoContent where
  toJSON _ = object []
