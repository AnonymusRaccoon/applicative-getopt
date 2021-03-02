{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data
import Data.Typeable


data Configuration = Configuration {
  rule :: Int,
  start :: Maybe Int,
  lines :: Maybe Int,
  window :: Maybe Int,
  move :: Maybe Int
} deriving (Show, Data, Typeable)

argsName :: [String]
argsName = constrFields . head . dataTypeConstrs  $ dataTypeOf (undefined :: Configuration)

argsType :: [TypeRep]
argsType = recurse $ typeOf Configuration
  where recurse x = let (_, y) = splitTyConApp x in
                    case y of (z:zs:[]) -> z : recurse zs
                              []        -> []

args :: [(String, TypeRep)]
args = zip argsName argsType

--getOpt :: [String] -> Configuration
--getOpt [] ->
