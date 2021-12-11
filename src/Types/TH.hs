{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}
module Types.TH where

import Data.Aeson
import Data.UUID
import Language.Haskell.TH
import Universum
import Web.HttpApiData

import Data.Char        (toLower)
import Data.List        (stripPrefix)
import Data.Traversable (for)
import Types.TH.Classes

makeIsClass "Text"
makeIsClass "Int64"
makeIsClass "UUID"

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
                                                   <$> metaName) } |]
  return [DataD [] name [] Nothing conts
            [DerivClause (Just StockStrategy)
                    $ map ConT [ ''Eq , ''Show, ''Generic]]]
         `conc`
   [d| instance ToJSON $(conT name) where toJSON = genericToJSON $options |]
         `conc`
   [d| instance FromJSON $(conT name)where parseJSON = genericParseJSON $options
     |]

-- TODO IsText Enum instance
-- TODO deriveComposite
