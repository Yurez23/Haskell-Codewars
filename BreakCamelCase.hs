module BreakCamelCase where

{-
"camelCasing"  =>  "camel Casing"
"identifier"   =>  "identifier"
""             =>  ""
-}

import Data.Char ( isUpper )
import Data.List.Split ( split, startsWithOneOf )

solution :: String -> String
solution [] = []
solution (c:cs) = c : foldr (\c acc -> if isUpper c then ' ' : c : acc else c : acc) [] cs

solution' :: String -> String
solution' = unwords . split (startsWithOneOf ['A'..'Z'])