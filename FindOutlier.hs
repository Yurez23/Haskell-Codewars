module FindOutlier where

import Data.List ( groupBy, partition )

{-
[2, 4, 0, 100, 4, 11, 2602, 36] -> 11 
[160, 3, 1719, 19, 11, 13, -21] -> 160
-}

findOutlier :: [Int] -> Int 
findOutlier = head . helper where
  helper xs 
    | length grouped == 3 = grouped !! 1
    | length (head grouped) == 1 = head grouped
    | otherwise = grouped !! 1
    where
      grouped = groupBy equalParity xs

equalParity :: Int -> Int -> Bool
equalParity x y = odd x == odd y


findOutlier' :: [Int] -> Int
findOutlier' xs =
    case partition even xs of
        ([x], _) -> x
        (_, [x]) -> x
        otherwise -> error "invalid input"