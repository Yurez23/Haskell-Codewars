module PositiveSum where

positiveSum :: [Int] -> Int
positiveSum xs = sum [x | x <- xs, x > 0]

positiveSum' :: [Int] -> Int
positiveSum' = sum . filter (>0)