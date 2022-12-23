{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Codewars.Parentheses where

  
validParentheses :: String -> Bool
validParentheses = null . helper . filter (`elem` "()")

helper = foldr foldFunc ""
    where
        foldFunc ')' ps = ')' : ps
        foldFunc '(' (')':ps) = ps
        foldFunc '(' ps = '(' : ps

validParentheses' :: String -> Bool
validParentheses' s = parse s 0

parse :: String -> Int -> Bool
parse [] n = n == 0
parse (x:xs) n
  | n < 0 = False
  | otherwise = case x of
                  '(' -> parse xs (n + 1)
                  ')' -> parse xs (n - 1)

test1 = validParentheses "()"
test2 = not (validParentheses ")(()))")
test3 = not (validParentheses "(")
test4 = validParentheses "(())((()())())"
test5 = validParentheses "()"
test6 = not (validParentheses ")(")
test7 = not (validParentheses ")")
test8 = validParentheses "(())((()())())"
testAll = test1 && test2 && test3 && test4 && test5 && test6 && test7 && test8