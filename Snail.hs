module Snail where

import Data.List ( transpose )

snail :: [[Int]] -> [Int]
snail [[]] = []
snail [] = []
snail [[x]] = [x]
snail ms = 
  let 
    ((h:t)) = ms
    l = last ms
    m = init t
  in
    h ++ map last m ++ reverse l ++ reverse (map head m) ++ snail (insideMatrix ms) 

insideMatrix :: [[a]] -> [[a]]
insideMatrix = map (init . tail) . init . tail

{-
 1  2  3  4
 5  6  7  8
 9 10 11 12
13 14 15 16

>>> a = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
>>> insideMatrix a
[[6,7],[10,11]]

1 2 3
4 5 6
7 8 9

>>> b = [[1,2,3],[4,5,6],[7,8,9]]
>>> insideMatrix b
[[5]]

>>> c = [[1,2],[3,4]]
>>> insideMatrix c
[]
-}

snail' :: [[Int]] -> [Int]
snail' [] = []
snail' (xs:xss) = xs ++ (snail' . reverse . transpose) xss

{-
 1  2  3  4
 5  6  7  8
 9 10 11 12
13 14 15 16

 5  6  7  8
 9 10 11 12
13 14 15 16

5  9 13
6 10 14
7 11 15
8 12 16

8 12 16
7 11 15
6 10 14
5  9 13

8 12 16 15 14 13 9 5 6 7 11 10 9

>>> a = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
>>> snail $ reverse $ transpose $ tail a
[8,12,16,15,14,13,9,5,6,7,11,10]

1 2 3
4 5 6
7 8 9

>>> b = [[1,2,3],[4,5,6],[7,8,9]]
>>> insideMatrix b
[[5]]

>>> c = [[1,2],[3,4]]
>>> insideMatrix c
[]
-}
