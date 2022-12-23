module HighAndLow where

highAndLow :: String -> String
highAndLow =
  (\(l, h) -> show h ++ " " ++ show l)
    . (\xs -> (minimum xs, maximum xs))
    . map (read :: String -> Int)
    . words
