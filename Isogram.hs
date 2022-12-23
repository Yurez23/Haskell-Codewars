module Isogram where

import Data.Char (toLower)
import Data.List ( (\\) )

isIsogram :: String -> Bool
isIsogram = fst . foldr (foldFunc . toLower) (True, []) where
  foldFunc c (res, acc) = (notElem c acc && res, c:acc)


isIsogram' :: String -> Bool
isIsogram' str = null $ map toLower str \\ ['a'..'z']