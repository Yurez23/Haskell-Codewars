module Gps1 where

{-
s = 15
x =  [0.0, 0.19,  0.5, 0.75,  1.0, 1.25,  1.5, 1.75,  2.0, 2.25]
gps s x = [45.6, 74.4, 60.0, 60.0, 60.0, 60.0, 60.0, 60.0, 60.0]
-}

gps :: Int -> [Double] -> Int
gps s x 
  | length x <= 1 = 0
  | otherwise = floor $ maximum $ map (\x -> 3600 * x / fromIntegral s) deltaDistances where 
    deltaDistances = zipWith (-) (tail x) (init x)

-- (3600 * delta_distance) / s