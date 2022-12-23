module Accumule where

import Data.Char

{-
accum("abcd") -> "A-Bb-Ccc-Dddd"
accum("RqaEzty") -> "R-Qq-Aaa-Eeee-Zzzzz-Tttttt-Yyyyyyy"
accum("cwAt") -> "C-Ww-Aaa-Tttt"
-}

accum :: [Char] -> [Char]
accum cs =
  let (c' : cs') = map (\(c : cs) -> toUpper c : cs) $ fst $ foldr ((\c (acc, n) -> (replicate n c : acc, n - 1)) . toLower) ([], length cs) cs
   in c' ++ concatMap ('-' :) cs'



{-
>>> accum "abcd"
"A-Bb-Ccc-Dddd"

>>> accum("RqaEzty")
"R-Qq-Aaa-Eeee-Zzzzz-Tttttt-Yyyyyyy"

>>> accum("cwAt")
"C-Ww-Aaa-Tttt"
-}
