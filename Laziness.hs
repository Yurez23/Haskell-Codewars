module Laziness where

import Data.List ( find )

type Matrix = [[Bool]]

generate :: Int -> Int -> Matrix
generate n m =
  let falses = repeat False
      oneTrue = replicate m False ++ [ True ] ++ falses
  in replicate n falses ++ [oneTrue] ++ repeat falses

findTrue :: Matrix -> (Int, Int)
findTrue ms 
  = (\(Just (_,x,y)) -> (x,y)) 
  $ find (\(b,_,_) -> b) 
  $ map (\(x, y) -> (matrixIndexing ms x y, x, y)) 
  $ concat [(n, n) : [(n, y) | y <- [0..n-1]] ++ [(x, n) | x <- [0..n-1]] | n <- [0..]]

matrixIndexing :: [[a]] -> Int -> Int -> a
matrixIndexing ms i j = ms !! i !! j

matrixElemIndex :: Eq a => [[a]] -> a -> (Int, Int)
matrixElemIndex ms x = helper (0, 0) where
  helper (i, j) 
    | matrixIndexing ms i j == x = (i, j)
    | i >= j = helper (i, j + 1)
    | otherwise = helper (i + 1, j)

findTrue' :: Matrix -> (Int, Int)
findTrue' m = head [(x,y)|i<-[0..],x<-[0..i],let y=i-x,m !! x !! y]

evens :: [a] -> [a]
evens []  = []
evens [x] = [x]
evens (x:_:xs) = x : evens xs

odds :: [a] -> [a]
odds []  = []
odds [x] = []
odds (_:x:xs) = x : odds xs

cleave :: [a] -> ([a], [a])
cleave xs = (evens xs, odds xs)

-- cleave [1,2,3,4] =
--   ~> (evens [1,2,3,4], odds [1,2,3,4])
--   ~> (1 : evens [3,4], odds [1,2,3,4])
--   ~> (1 : 3 : evens [], odds [1,2,3,4])
--   ~> ([1,3], odds [1,2,3,4])
--   ~> ([1,3], 2 : odds [3,4])
--   ~> ([1,3], 2 : 4 : odds [])
--   ~> ([1,3], [2,4]])

cleave2 :: [a] -> ([a], [a])
cleave2 = cleave' ([],[]) where
  cleave' (eacc,oacc) [] = (eacc,oacc)
  cleave' (eacc,oacc) [x] = (x:eacc,oacc)
  cleave' (eacc,oacc) (x:x':xs) = cleave' (x:eacc,x':oacc) xs

-- cleave2 [1,2,3,4] =
--   ~> cleave' ([],[]) [1,2,3,4]
--   ~> cleave' ([1], [2]) [3,4]
--   ~> cleave' ([3,1], [4,2]) []
--   ~> ([3,1], [4,2])

