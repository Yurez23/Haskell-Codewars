module Reverse where

import Data.List (groupBy)
import Data.List.Split (oneOf, split)

{-
"This is an example!" ==> "sihT si na !elpmaxe"
"double  spaces"      ==> "elbuod  secaps"
-}

reverseWords :: String -> String
reverseWords = concatMap reverse . groupBy (\a b -> not (a /= ' ' && b == ' ' || a == ' ' && b /= ' '))

reverseWords' :: String -> String
reverseWords' xs = concatMap reverse (split (oneOf " ") xs)