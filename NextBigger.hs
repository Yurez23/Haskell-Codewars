module NextBigger (nextBigger) where

import Data.List ( delete, permutations, sortBy, sort )

nextBigger :: Int -> Int
nextBigger = fromDigits . nextBiggerDigits . digits

nextBigger' :: Int -> Int
nextBigger' x = findNextBigger $ digitPermutations x where
  digitPermutations = map read . permutations . show
  findNextBigger = foldr foldFunc (-1) where
    foldFunc x' acc
      | x' <= x = acc
      | x' < acc || acc == -1 = x'
      | otherwise = acc

minimumWithFilter :: Ord a => (a -> Bool) -> [a] -> a
minimumWithFilter p = foldl1 foldFunc . dropWhile (not . p) where
  foldFunc acc x
    | p x && x < acc = x
    | otherwise = acc

digits :: Int -> [Int]
digits x
  | x < 10 = [x]
  | otherwise = x `mod` 10 : digits (x `div` 10)

fromDigits :: [Int] -> Int
fromDigits = fst . foldl foldFunc (0, 0) where
  foldFunc (acc, n) x = (x * 10^n + acc, succ n)

nextBiggerDigits :: [Int] -> [Int]
nextBiggerDigits = helper [] where
  helper acc (x:y:xs)
    | x > y = let d = minimumWithFilter (>y) (x:acc) in reverseSort (delete d (x:y:acc)) ++ [d] ++ xs
    | otherwise = helper (x:acc) (y:xs)
  helper _ _ = [-1]

reverseSort :: Ord a => [a] -> [a]
reverseSort = sortBy compareFunc where
  compareFunc x y
    | x < y = GT
    | x > y = LT
    | otherwise = EQ

nextBigger'' :: Int -> Int
nextBigger'' x
  | reverse (sort $ show x) == show x = -1
  | otherwise = nextBigger' x (x+1)
  where nextBigger' a b
          | sameDigits a b = b
          | otherwise = nextBigger' a (b+1)


sameDigits :: Int -> Int -> Bool
sameDigits a b = sort (show a) == sort (show b)

{-
>>> map (fromDigits . nextBiggerDigits . digits) [12,513,2017,9,111,531,414,144,10^9 + 1,370]
[21,531,2071,-1,-1,-1,441,414,1000000010,703]
-}

{-
>>> map nextBigger' [12,513,2017,9,111,531,414,144,10^9 + 1,370]
[21,531,2071,-1,-1,-1,441,414,1000000010,703]
-}


