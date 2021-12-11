{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
module Types.TH.Classes where

import Universum
import Language.Haskell.TH

makeIsClass :: String -> DecsQ
makeIsClass metaName = let className = mkName $ "Is" <> metaName;
                           fromName  = mkName $ "from" <> metaName;
                           toName    = mkName $ "to"  <> metaName;
                           var       = mkName "a";
                           idName    = mkName "id";
                           name      = mkName metaName;
                   in return [ClassD [] className [PlainTV var] [] [
               SigD fromName (AppT (AppT ArrowT (ConT name)) (VarT var))
             , SigD toName   (AppT (AppT ArrowT (VarT var)) (ConT name))
                                                                   ]
          ,  InstanceD Nothing [] (AppT (ConT className) (ConT name))
            [ ValD (VarP fromName) (NormalB (VarE idName)) []
            , ValD (VarP toName)   (NormalB (VarE idName)) []]
          ]
