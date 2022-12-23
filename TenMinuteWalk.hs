{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TenMinuteWalk where

import Data.List ( delete )

{-
isValidWalk ['n','s','n','s','n','s','n','s','n','s'] ??? "should return True  on valid walk"
isValidWalk ['n','s','n','s','n','s','n','s','n','n'] ??! "should return False on invalid walk"
isValidWalk ['n','s']    ??! "should return False on too short walk"
isValidWalk (repeat 'n') ??! "should return False on infinite walk"
isValidWalk ['n','s','e','w','n','s','e','w','n','s'] ??? "should return True on valid walk"
-}

isValidWalk :: [Char] -> Bool
isValidWalk walk
  | isLength 10 walk = null $ foldr foldFunc [] walk
  | otherwise = False
  where
    foldFunc 'n' acc
      | 's' `elem` acc = delete 's' acc
      | otherwise = 'n' : acc
    foldFunc 's' acc
      | 'n' `elem` acc = delete 'n' acc
      | otherwise = 's' : acc
    foldFunc 'e' acc
      | 'w' `elem` acc = delete 'w' acc
      | otherwise = 'e' : acc
    foldFunc 'w' acc
      | 'e' `elem` acc = delete 'e' acc
      | otherwise = 'w' : acc

isLength :: Int -> [a] -> Bool
isLength n xs = all (<= n) (zipWith const [1 ..] xs) && length xs == n

-- f ('w', 1) (f ('w', 2) (f ('w', 3) (Just 0))) =
--   f ('w', 2) (f ('w', 3) (Just 0)) >>= \x -> Just (x + 1)
--   ((Just 0 >>= \x'' -> Just (x'' + 1)) >>= \x' -> Just (x' + 1)) >>= \x -> Just (x + 1)