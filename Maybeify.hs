{-# LANGUAGE TemplateHaskell #-}
module Maybeify (maybeify) where

import Language.Haskell.TH
import Data.Char (toUpper, toLower)

maybeify :: Name -> [Name] -> Q [Dec]
maybeify name derive = do
    TyConI (DataD ctx _ tyVarBndrs constructors _) <- reify name
    return [DataD ctx
                  (prependMaybe name)
                  tyVarBndrs
                  (map maybeifyConstructor constructors)
                  derive]

maybeifyConstructor :: Con -> Con
maybeifyConstructor (NormalC name fields) =
    NormalC (prependMaybe name) $ map (\(s, t) ->
      (s, AppT (ConT ''Maybe) t)) fields
maybeifyConstructor (RecC name fields) =
    RecC (prependMaybe name) $ map (\(n, s, t) ->
      (prependmaybe n, s, AppT (ConT ''Maybe) t)) fields

prependMaybe :: Name -> Name
prependMaybe name = mkName $ "Maybe" ++ nameBase name

prependmaybe :: Name -> Name
prependmaybe name = mkName $ "maybe" ++ capitalize (nameBase name)
    where
        capitalize [] = []
        capitalize (c:cs) = toUpper c : map toLower cs
