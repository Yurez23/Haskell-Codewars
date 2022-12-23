module TitleCase where

import Data.Char ( toUpper, toLower )

{-
titleCase "a an the of" "a clash of KINGS" -- should return: "A Clash of Kings"
titleCase "The In" "THE WIND IN THE WILLOWS" -- should return: "The Wind in the Willows"
titleCase "" "the quick brown fox" -- should return: "The Quick Brown Fox"
-}

titleCase :: String -> String -> String
titleCase _ [] = []
titleCase minor title = unwords $ capitalise (head titleWords) : foldr foldFunc [] (tail titleWords) where
  capitalise [] = []
  capitalise (c:cs) = toUpper c : cs
  minorWords = words $ map toLower minor
  titleWords = words $ map toLower title
  foldFunc word acc
    | word `elem` minorWords = word : acc
    | otherwise = capitalise word : acc

