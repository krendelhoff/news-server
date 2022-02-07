{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module Types.TH.Classes where

import Data.Char           (toLower)
import Data.List           (stripPrefix)
import Data.Traversable    (for)
import Language.Haskell.TH
import Universum

decapitalize :: String -> String
decapitalize []     = []
decapitalize (x:xs) = toLower x : xs

stripDecapitalizeModifier :: [Char] -> [Char] -> String
stripDecapitalizeModifier prefix metaName = decapitalize $ stripModifier prefix metaName

stripModifier :: [Char] -> [Char] -> String
stripModifier metaName (stripPrefix metaName -> Just x) = x
stripModifier _ x                                       = x

makeIsClass' :: (String -> String) -> String -> DecsQ
makeIsClass' modifier metaName@(modifier -> modMetaName) =
  let className = mkName $ "Is" <> modMetaName;
      fromName  = mkName $ "from" <> modMetaName;
      toName    = mkName $ "to"  <> modMetaName;
      var       = mkName "a";
      name      = mkName metaName;
  in return [ClassD [] className [PlainTV var] []
      [ SigD fromName (AppT (AppT ArrowT (ConT name)) (VarT var))
      , SigD toName   (AppT (AppT ArrowT (VarT var)) (ConT name)) ]
      , InstanceD Nothing [] (AppT (ConT className) (ConT name))
      [ ValD (VarP fromName) (NormalB (VarE 'id)) []
      , ValD (VarP toName)   (NormalB (VarE 'id)) [] ]]

makeIsClass :: String -> DecsQ
makeIsClass = makeIsClass' id

makeKnown' :: (String -> String) -> Name -> DecsQ
makeKnown' modifier metaName = do
  (TyConI (DataD [] _ [] Nothing cons [])) <- reify metaName
  names <- for cons \case
    NormalC name [] -> return name
    _               -> fail "Use only for enumeration types"
  return $ [ClassD [] className [KindedTV (mkName "a") (ConT metaName)]  []
            [ SigD functionName (ConT metaName) ]
           ] <> createInstances names
  where
    className = mkName $ "Known" <> modifier (nameBase metaName)
    functionName = mkName $ decapitalize (modifier (nameBase metaName)) <> "Val"
    createInstances = map \mName -> let name = mkName (nameBase mName) in
      InstanceD Nothing [] (AppT (ConT className) (PromotedT name))
        [ ValD (VarP functionName) (NormalB (ConE name)) [] ]

makeKnown :: Name -> DecsQ
makeKnown = makeKnown' id
