module Term where

{-
[12, 10, 8, 12, 7, 6, 4, 10, 12]              -->  12
[12, 10, 8, 12, 7, 6, 4, 10, 12, 10]          -->  12
[12, 10, 8, 8, 3, 3, 3, 3, 2, 4, 10, 12, 10]  -->   3
-}

import Data.List ( maximumBy, group, sort )
import Data.Function ( on )
import Data.Ord (comparing)

highestRank :: Ord c => [c] -> c
highestRank = fst . maximumBy (compare `on` snd) . map (\x -> (head x, length x)) . group . sort

highestRank' :: Ord c => [c] -> c
highestRank' = head . maximumBy (comparing length) . group . sort