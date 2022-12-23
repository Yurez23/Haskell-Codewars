module UnlimitedGameOfLife where

import Data.Function (on)
import Data.List (groupBy, maximumBy, minimumBy, sortOn)

type Coord = (Int, Int)

type Field = [[Int]]

type ListWithCoords = [(Int, Coord)]

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration xss n = listToField $ iterate oneGen (fieldToList xss) !! n
  where
    (maxI, maxJ) = (length xss, length (head xss))

fieldToList :: Field -> ListWithCoords
fieldToList xss = concat $ zipWith (\xs i -> zipWith (\x j -> (x, (i, j))) xs [1 ..]) xss [1 ..]

listToField :: ListWithCoords -> Field
listToField = fmap (fmap fst) . groupBy (\(_, (i, _)) (_, (i', _)) -> i == i')

addZeros :: ListWithCoords -> (Int, Int) -> (Int, Int) -> ListWithCoords
addZeros xss (minI, maxI) (minJ, maxJ) = 
  [(0, (minI - 1, j)) | j <- [minJ - 1 .. maxJ + 1]] ++
  [(0, (maxI + 1, j)) | j <- [minJ - 1 .. maxJ + 1]] ++
  [(0, (i, minJ - 1)) | i <- [minI .. maxI]] ++
  [(0, (i, maxJ + 1)) | i <- [minI .. maxI]] ++  
  xss

getNeighbours :: ListWithCoords -> Coord -> ListWithCoords
getNeighbours xs (i, j) =
  filter
    (\(x, cs) -> cs `elem` [(k, l) | k <- [i -1, i, i + 1], l <- [j -1, j, j + 1], (k, l) /= (i, j)])
    xs

sumBy :: (Foldable t, Num b) => (a -> b) -> t a -> b
sumBy f = foldr (\x acc -> f x + acc) 0

sumVals :: ListWithCoords -> Int
sumVals = sumBy fst

oneGen :: ListWithCoords -> ListWithCoords
oneGen xs = map liveFunc xs
  where
    liveFunc (x, cs)
      | x == 0 && neighboursNumber == 3 = (1, cs)
      | x == 1 && neighboursNumber `elem` [2, 3] = (1, cs)
      | otherwise = (0, cs)
      where
        neighboursNumber = sumVals $ getNeighbours xs cs

a :: Field
a =
  [ [1, 0, 0],
    [0, 1, 1],
    [1, 1, 0]
  ]

a' :: ListWithCoords
a' = fieldToList a

b :: [Coord]
b = [(x, y) | x <- [1, 2, 3], y <- [1, 2, 3]]

gliders :: [Field]
gliders =
  [ [ [1, 0, 0],
      [0, 1, 1],
      [1, 1, 0]
    ],
    [ [0, 1, 0],
      [0, 0, 1],
      [1, 1, 1]
    ],
    [ [1, 0, 1],
      [0, 1, 1],
      [0, 1, 0]
    ],
    [ [0, 0, 1],
      [1, 0, 1],
      [0, 1, 1]
    ]
  ]

{-
>>> listToField $ sortOn (fst . snd) $ sortOn (snd . snd) $ addZeros a' (1,3) (1,3)
[[0,0,0,0,0],[0,1,0,0,0],[0,0,1,1,0],[0,1,1,0,0],[0,0,0,0,0]]
-}
