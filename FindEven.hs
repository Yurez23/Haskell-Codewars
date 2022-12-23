module FindEven where

import Data.List ( elemIndex, findIndex ) 
import Data.Maybe (fromMaybe)

{-
findEvenIndex [1,2,3,4,3,2,1] = 3
findEvenIndex [1,100,50,-51,1,1] = 1
findEvenIndex [20,10,-80,10,10,15,35] = 0
-}

findEvenIndex :: [Int] -> Int
findEvenIndex arr 
  = fromMaybe (-1) 
  $ findIndex (\(l, r) -> sum l == sum r) 
  $ zipWith ($) (map (\n -> (\(l, r) -> (l, tail r)) . splitAt n) [0..]) (replicate (length arr) arr)

findEvenIndex' :: [Int] -> Int
findEvenIndex' = fromMaybe (-1) . elemIndex True . (zipWith (==) <$> scanl1 (+) <*> scanr1 (+))