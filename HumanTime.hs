module HumanTime where

import Text.Printf

humanReadable :: Int -> String
humanReadable = (\(h, m, s) -> h ++ ':' : m ++ ':' : s) . tripleMap paddedTo2Digits . secondsToHMS

secondsToHMS :: Int -> (Int, Int, Int)
secondsToHMS x = (h, m, s)
  where
    (x', s) = divMod x 60
    (h, m) = divMod x' 60

paddedTo2Digits :: Int -> String
paddedTo2Digits x
  | x < 10 = '0' : show x
  | otherwise = show x

tripleMap :: (a -> b) -> (a, a, a) -> (b, b, b)
tripleMap f (x, y, z) = (f x, f y, f z)

tripleFold :: (a -> a -> a) -> (a, a, a) -> a
tripleFold f (x, y, z) = x `f` y `f` z

humanReadable' :: Int -> String
humanReadable' x = printf "%02d:%02d:%02d" h m s
  where
    (y, s) = x `divMod` 60
    (h, m) = y `divMod` 60

test1 = humanReadable 0 == "00:00:00"

test2 = humanReadable 59 == "00:00:59"

test3 = humanReadable 60 == "00:01:00"

test4 = humanReadable 90 == "00:01:30"

test5 = humanReadable 86399 == "23:59:59"

test6 = humanReadable 359999 == "99:59:59"

testAll = test1 && test2 && test3 && test4 && test5 && test6