module ProductFib where

import Data.List (elemIndex)

{-
productFib 714 -> should return (21, 34, true), 
               -> since F(8) = 21, F(9) = 34 and 714 = 21 * 34

productFib 800 -> should return (34, 55, false), 
               -> since F(8) = 21, F(9) = 34, F(10) = 55 and 21 * 34 < 800 < 34 * 55
-}

-- | Returns a pair of consecutive Fibonacci numbers a b,
--   where (a*b) is equal to the input, or proofs that the
--   number isn't a product of two consecutive Fibonacci 
--   numbers.

-- productFib :: Integer -> (Integer, Integer, Bool)
productFib n = helper fibs' where
  helper (prev:curr:fs) 
    | prev * curr == n = (prev, curr, True)
    | prev * curr > n = (prev, curr, False)
    | otherwise = helper (curr:fs)

-- maybe (0, 0, False) (\n' -> (head (tail (fibs n')), head (fibs n'), True)) $ 
fibs :: Integer -> [Integer]
fibs n | n < 0 = error "Negative number"
fibs 0 = [0]
fibs 1 = [1, 0]
fibs n = let prevs@(prev : prevprev : _) = fibs (n - 1) in prev + prevprev : prevs 

fibs' :: [Integer]
fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')

productFib' :: Integer -> (Integer, Integer, Bool)
productFib' n = go 0 1 n
  where
    go a b c
        | a * b >= c = (a, b, a * b == c)
        | otherwise = go b (a + b) c