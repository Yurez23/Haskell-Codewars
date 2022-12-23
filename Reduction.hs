module Reduction where

data Direction = North | East | West | South deriving (Eq, Show)



dirReduce :: [Direction] -> [Direction]
dirReduce = findFirstIdentical . iterate helper


helper (North:South:xs) = helper xs
helper (South:North:xs) = helper xs
helper (East:West:xs) = helper xs
helper (West:East:xs) = helper xs
helper (x:y:xs) = x:helper (y:xs)
helper [x] = [x]
helper [] = []


findFirstIdentical (x:y:xs)
    | x == y = x
    | otherwise = findFirstIdentical (y:xs)



dirReduce' :: [Direction] -> [Direction]
dirReduce' = foldr collapse []

collapse North (South:xs) = xs
collapse South (North:xs) = xs
collapse East (West:xs) = xs
collapse West (East:xs) = xs
collapse x xs = x:xs



test1 = dirReduce [] == []
test2 = dirReduce [North] == [North]
test3 = dirReduce [North, West] == [North,West]
test4 = dirReduce [North, West, East] == [North]
test5 = dirReduce [North, West, South, East] == [North, West, South, East]
test6 = dirReduce [North, South, South, East, West, North, West] == [West]
test7 = dirReduce [North, South, South, East, West, North] == []
testAll = test1 && test2 && test3 && test4 && test5 && test6 && test7