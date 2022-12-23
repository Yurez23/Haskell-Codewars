module Braces where

import Data.List (delete)

{-
"(){}[]"   =>  True
"([{}])"   =>  True
"(}"       =>  False
"[(])"     =>  False
"[({})](]" =>  False
-}

validBraces :: String -> Bool
validBraces = (\(b, s) -> b && null s) . foldl foldFunc (True, []) where
  foldFunc (res, []) c 
    | isOpen c = (res, [c])
    | otherwise = (False, []) 
  foldFunc (res, c':acc) c 
    | isOpen c = (res, c : c' : acc)
    | isClose c && oppositeBracket c == c' = (res, acc)
    | otherwise = (False, c' : acc)

  isOpen = (`elem` "([{")
  isClose = (`elem` ")]}")
  oppositeBracket '(' = ')' 
  oppositeBracket '[' = ']' 
  oppositeBracket '{' = '}' 
  oppositeBracket ')' = '(' 
  oppositeBracket ']' = '[' 
  oppositeBracket '}' = '{' 

data Brace = Open BraceType | Close BraceType deriving (Eq, Show)
data BraceType = Paren | Bracket | CurlyBracket deriving (Eq, Show)

charToBrace :: Char -> Brace
charToBrace '(' = Open Paren
charToBrace ')' = Close Paren
charToBrace '[' = Open Bracket
charToBrace ']' = Close Bracket
charToBrace '{' = Open CurlyBracket
charToBrace '}' = Close CurlyBracket

validBraces' :: String -> Bool
validBraces' = go [] . map charToBrace
  where go (Open x:xs) (Close y:ys) = x == y && go xs ys
        go xs (Open y:ys) = go (Open y:xs) ys
        go [] [] = True
        go _ _ = False


validBraces'' :: String -> Bool
validBraces'' = null . foldr collapse [] where
  collapse '(' (')':xs) = xs
  collapse '{' ('}':xs) = xs
  collapse '[' (']':xs) = xs
  collapse x xs = x:xs

-- validBraces'' "(){}[]" ==
--   ~> foldr collapse [] "(){}[]" 
--   ~> collapse '(' (foldr collapse [] "){}[]")
--   ~> collapse '(' (collapse ')' (foldr collapse [] "{}[]"))
--   ~> collapse '(' (collapse ')' (collapse '{' (foldr collapse [] "}[]")))
--   ~> collapse '(' (collapse ')' (collapse '{' (collapse '}' (foldr collapse [] "[]"))))
--   ~> collapse '(' (collapse ')' (collapse '{' (collapse '}' (collapse '[' (foldr collapse [] "]")))))
--   ~> collapse '(' (collapse ')' (collapse '{' (collapse '}' (collapse '[' (collapse ']' (foldr collapse [] ""))))))
--   ~> collapse '(' (collapse ')' (collapse '{' (collapse '}' (collapse '[' (collapse ']' [])))))
--   ~> collapse '(' (collapse ')' (collapse '{' (collapse '}' (collapse '[' "]"))))
--   ~> collapse '(' (collapse ')' (collapse '{' (collapse '}' [])))
--   ~> collapse '(' (collapse ')' (collapse '{' '}':[]))
--   ~> collapse '(' (collapse ')' [])
--   ~> collapse '(' ')':[]
--   ~> []