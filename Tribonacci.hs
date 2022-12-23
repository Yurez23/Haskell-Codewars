module Tribonacci where

{-
tribonacci (1, 1, 1) 10 `shouldBe` [1,1,1,3,5,9,17,31,57,105]
tribonacci (0, 0, 1) 10 `shouldBe` [0,0,1,1,2,4,7,13,24,44]
tribonacci (0, 1, 1) 10 `shouldBe` [0,1,1,2,4,7,13,24,44,81]
-}

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n | n < 3 = take n [a, b, c]
tribonacci (a, b, c) n = reverse $ last $ take (n - 2) $ iterate step [c, b, a] where
  step (a:b:c:xs) = [a + b + c, a, b, c] ++ xs 

tribonacci' :: Num a => (a, a, a) -> Int -> [a]
tribonacci' _ n | n < 1 = []
tribonacci' (a, b, c) n = a : tribonacci (b, c, a+b+c) (n-1)