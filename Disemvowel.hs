module Disemvowel where

vowels :: String
vowels = "AEIOUaeiou"

disemvowel :: String -> String
disemvowel = filter (`notElem` vowels)