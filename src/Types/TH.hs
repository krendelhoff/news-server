{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}
module Types.TH where

import Data.Aeson
import Data.Char           (toLower)
import Data.List           (stripPrefix)
import Data.Traversable    (for)
import Data.UUID           hiding (toText)
import Language.Haskell.TH
import Universum           hiding (toText)
import Web.HttpApiData
import Data.Time

import qualified Data.Text as T


import Types.TH.Classes
import qualified Data.ByteString as B
import qualified Data.Aeson as B

makeIsClass "Text"
makeIsClass "Int64"
makeIsClass "UUID"
makeIsClass "Bool"
makeIsClass "UTCTime"
makeIsClass "ByteString"

conc :: DecsQ -> DecsQ -> DecsQ
conc = liftA2 (<>)

newTextType :: String -> DecsQ
newTextType metaName = let name = mkName metaName in
                         return [NewtypeD [] name [] Nothing
                           (NormalC name [
                               (Bang NoSourceUnpackedness NoSourceStrictness
                               , ConT ''Text) ]) [
                           DerivClause (Just NewtypeStrategy) $
                             map ConT [ ''Eq
                                      , ''Ord
                                      , ''Show
                                      , ''ToJSON
                                      , ''FromJSON
                                      , ''ToHttpApiData
                                      , ''FromHttpApiData
                                      , ''Semigroup
                                      , ''Monoid
                                      , ''IsString
                                      , ''IsText
                                      ] ]]

newIntType :: String -> DecsQ
newIntType metaName = let name = mkName metaName in
                         return [NewtypeD [] name [] Nothing
                           (NormalC name [
                               (Bang NoSourceUnpackedness NoSourceStrictness
                               , ConT ''Int64) ]) [
                           DerivClause (Just NewtypeStrategy) $
                             map ConT [ ''Eq
                                      , ''Show
                                      , ''Bounded
                                      , ''Enum
                                      , ''Ord
                                      , ''ToJSON
                                      , ''FromJSON
                                      , ''ToHttpApiData
                                      , ''FromHttpApiData
                                      , ''Semigroup
                                      , ''Monoid
                                      , ''IsInt64
                                      ] ]]

newUUIDType :: String -> DecsQ
newUUIDType metaName = let name = mkName metaName in
                         return [NewtypeD [] name [] Nothing
                           (NormalC name [
                               (Bang NoSourceUnpackedness NoSourceStrictness
                               , ConT ''UUID) ]) [
                           DerivClause (Just NewtypeStrategy) $
                             map ConT [ ''Eq
                                      , ''Show
                                      , ''ToJSON
                                      , ''FromJSON
                                      , ''ToHttpApiData
                                      , ''FromHttpApiData
                                      , ''IsUUID
                                      ] ]]
-- maybe TODO IsText (uuid parse function + toText from Data.UUID)

newBoolType :: String -> DecsQ
newBoolType metaName = let name = mkName metaName in
                         return [NewtypeD [] name [] Nothing
                           (NormalC name [
                               (Bang NoSourceUnpackedness NoSourceStrictness
                               , ConT ''Bool) ]) [
                           DerivClause (Just NewtypeStrategy) $
                             map ConT [ ''Eq
                                      , ''Show
                                      , ''Generic
                                      , ''Ord
                                      , ''Bounded
                                      , ''Enum
                                      , ''ToJSON
                                      , ''FromJSON
                                      , ''ToHttpApiData
                                      , ''FromHttpApiData
                                      , ''IsBool
                                      ] ]]

-- FIXME REALLY, REALLY BAD FUNCTIONS
instance FromJSON ByteString where
  parseJSON (String s) = return $ encodeUtf8 s
  parseJSON _ = fail "Wrong format bytestring!"

instance ToJSON ByteString where
  toJSON = String . decodeUtf8

newByteaType :: String -> DecsQ
newByteaType metaName = let name = mkName metaName in
                         return [NewtypeD [] name [] Nothing
                           (NormalC name [
                               (Bang NoSourceUnpackedness NoSourceStrictness
                               , ConT ''ByteString) ]) [
                           DerivClause (Just NewtypeStrategy) $
                             map ConT [ ''Eq
                                      , ''Ord
                                      , ''Show
                                      , ''Semigroup
                                      , ''Monoid
                                      , ''IsString
                                      , ''ToJSON
                                      , ''FromJSON
                                      , ''IsByteString
                                      ] ]]
-- TODO FromJSON, toJSON

newUTCTimeType :: String -> DecsQ
newUTCTimeType metaName = let name = mkName metaName in
                         return [NewtypeD [] name [] Nothing
                           (NormalC name [
                               (Bang NoSourceUnpackedness NoSourceStrictness
                               , ConT ''UTCTime) ]) [
                           DerivClause (Just NewtypeStrategy) $
                             map ConT [ ''Eq
                                      , ''Ord
                                      , ''Show
                                      , ''ToJSON
                                      , ''FromJSON
                                      , ''ToHttpApiData
                                      , ''FromHttpApiData
                                      , ''IsUTCTime
                                      ] ]]
 
decapitalize :: String -> String
decapitalize []     = []
decapitalize (x:xs) = toLower x : xs

modifier :: [Char] -> [Char] -> String
modifier metaName (stripPrefix metaName -> Just x) = decapitalize x
modifier _ x                                       = x

newEnumType :: String -> [String] -> DecsQ
newEnumType metaName elems = do
  for_ elems \elem -> if metaName `isPrefixOf` elem
                     then pass
                     else fail "Constructors are not correctry prefixed!"
  let conts = flip NormalC [] . mkName <$> elems
      name = mkName metaName
      options = [| defaultOptions { constructorTagModifier =
                                      modifier $(return $
                                                   ListE $ LitE . CharL
                                                   <$> metaName)
                                  , omitNothingFields = True
                                  , sumEncoding = UntaggedValue
                                  } |]
  return [DataD [] name [] Nothing conts
            [DerivClause (Just StockStrategy)
                    $ map ConT [ ''Eq , ''Show, ''Generic]]]
         `conc`
   [d| instance ToJSON $(conT name) where toJSON = genericToJSON $options |]
         `conc`
   [d| instance FromJSON $(conT name)where parseJSON = genericParseJSON $options
     |]
         `conc`
   [d| instance IsText $(conT name) where
         fromText x = case fromJSON (String x) of
           Success a -> a
           _         -> error "never happen"
         toText (toJSON -> String x) = x
         toText _                    = error "never happen"
     |]
         `conc`
   [d| instance ToHttpApiData $(conT name) where
         toUrlPiece = toText
     |]
         `conc`
   [d| instance FromHttpApiData $(conT name) where
         parseUrlPiece x = case fromJSON (String x) of
           Success a -> Right a
           Error _ -> Left . T.pack $ "parseUrlPiece for enum failed on "
                                   <> T.unpack x
                                   <> " of type "
                                   <> metaName
     |]

-- TODO deriveComposite