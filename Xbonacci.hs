module Xbonacci where

{-
xbonacci [0,1] 10 `shouldBe` [0,1,1,2,3,5,8,13,21,34]
xbonacci [1,1] 10 `shouldBe` [1,1,2,3,5,8,13,21,34,55]
xbonacci [0,0,0,0,1] 10 `shouldBe` [0,0,0,0,1,1,2,4,8,16]
xbonacci [1,0,0,0,0,0,1] 10 `shouldBe` [1,0,0,0,0,0,1,2,3,6]
-}

xbonacci :: Num a => [a] -> Int -> [a]
xbonacci _ 0 = [] 
xbonacci (a:as) n = a : xbonacci (as ++ [sum as + a]) (n - 1)







-- take n $ foldr (zipWith (+)) (repeat 0) $ helper () (length as) where 
--   helper as 1 = [as]
--   helper as n = drop (n - 1) as : helper as (n - 1)