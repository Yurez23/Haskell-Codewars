module MultiplesOf3And5 where

solution :: Integer -> Integer
solution number 
  | number > 0 = sum [n | n <- [1 .. number - 1], n `mod` 3 == 0 || n `mod` 5 == 0]
  | otherwise = 0

test1 = solution 10 == 23

test2 = solution 20 == 78

test3 = solution 200 == 9168

testAll = test1 && test2 && test3