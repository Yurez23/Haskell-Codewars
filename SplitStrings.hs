module SplitStrings where

{-
solution "abc" `shouldBe` ["ab", "c_"]
solution "abcdef" `shouldBe` ["ab", "cd", "ef"]
-}

solution :: String -> [String]
solution = splitIntoPairs . toEvenLength
  where
    toEvenLength string
      | odd (length string) = string ++ "_"
      | otherwise = string
    splitIntoPairs [] = []
    splitIntoPairs (c1 : c2 : cs) = [c1, c2] : splitIntoPairs cs

solution' :: String -> [String]
solution' [] = []
solution' [x] = [[x, '_']]
solution' (x : y : xs) = [x, y] : solution xs
