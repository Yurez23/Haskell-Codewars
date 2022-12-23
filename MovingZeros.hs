module MovingZeros (moveZeros) where

import Data.List (partition)

moveZeros :: [Int] -> [Int]
moveZeros = uncurry (++) . partition (/= 0)

test1 = moveZeros [1, 2, 0, 1, 0, 1, 0, 3, 0, 1] == [1, 2, 1, 1, 3, 1, 0, 0, 0, 0]
