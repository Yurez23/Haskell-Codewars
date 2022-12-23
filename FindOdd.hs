module FindOdd where
import Data.List (partition)
import Data.Bits (xor)

findOdd :: [Int] -> Int
findOdd (x:xs)
  | odd l = x
  | otherwise = findOdd rest
  where
    (samex, rest) = partition (==x) (x:xs)
    l = length samex

findOdd' :: [Int] -> Int
findOdd' = foldr1 xor

test1 = findOdd [7] == 7
test2 = findOdd [0] == 0
test3 = findOdd [1,1,2] == 2
test4 = findOdd [0,1,0,1,0] == 0
test5 = findOdd [1,2,2,3,3,3,4,3,3,3,2,2,1] == 4
testAll = test1 && test2 && test3 && test4 && test5