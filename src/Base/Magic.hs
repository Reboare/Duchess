{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
-- |Includes all the Template Haskell magic we all love/hate
module Base.Magic 
(
    genFind
)
where

import Language.Haskell.TH
import Data.Char
import Data.List

ftl :: String -> String
ftl [] = []
ftl (x:xs) = toLower x : xs

genFind :: Name -> String -> Q [Dec]
genFind nm sName = do
    let genFunName = mkName sName --The main function name 
    let eq = mkName $! "eq" ++ nameBase nm --An Equality function
    xs <- newName "xs"
    return [FunD eq [Clause [ConP nm [WildP]] (NormalB (ConE 'True)) [],Clause [WildP] (NormalB (ConE 'False)) []],
            FunD genFunName [Clause [VarP xs] (NormalB (AppE (AppE (VarE 'find) (VarE eq)) (VarE xs))) []]]