module CamelCase where

{-
camelCase("hello case"); // => "HelloCase"
camelCase("camel case word"); // => "CamelCaseWord"
-}

import Data.Char ( toUpper )

camelCase :: String -> String
camelCase = concatMap (\(c:cs) -> toUpper c : cs) . words