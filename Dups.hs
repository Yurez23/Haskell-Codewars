module Dups where

{-
"din"      =>  "((("
"recede"   =>  "()()()"
"Success"  =>  ")())())"
"(( @"     =>  "))(("
-}

import Data.Char
import Data.Maybe

duplicateEncode :: String -> String
duplicateEncode xs = map ((\n -> if n == 1 then '(' else ')') . (\x -> fromJust $ lookup x a)) ls
  where
    ls = map toLower xs
    a =
      foldr
        ( \x acc ->
            if any
              (\(x', n') -> (x', n') `elem` acc)
              [(x, n) | n <- [1 .. length ls]]
              then
                map
                  (\(y, n) -> if y == x then (y, n + 1) else (y, n))
                  acc
              else (x, 1) : acc
        )
        []
        ls

duplicateEncode' :: String -> String
duplicateEncode' xs = map encode xs' where
  xs' = map toLower xs
  encode c = if length (filter (== c) xs') > 1 then ')' else '('