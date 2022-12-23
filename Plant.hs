module Plant where

{-
growingPlant 100 10 910 -> 10
After day 1 --> 100
After night 1 --> 90
After day 2 --> 190
After night 2 --> 180
After day 3 --> 280
After night 3 --> 270
After day 4 --> 370
After night 4 --> 360
After day 5 --> 460
After night 5 --> 450
After day 6 --> 550
After night 6 --> 540
After day 7 --> 640
After night 7 --> 630
After day 8 --> 730
After night 8 --> 720
After day 9 --> 820
After night 9 --> 810
After day 10 --> 910 
-}

growingPlant :: Int -> Int -> Int -> Int
growingPlant upSpeed downSpeed desiredHeight = snd $ head $ dropWhile ((< desiredHeight) . fst) [(n * upSpeed - (n - 1) * downSpeed, n) | n <- [1..]]
