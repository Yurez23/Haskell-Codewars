module ColouredTriangle where

triangle :: String -> String
triangle [] = undefined 
triangle [c] = [c]
triangle cs = triangle $ zipWith combineColors cs (tail cs)

combineColors :: Char -> Char -> Char
combineColors x y | x == y = x
combineColors 'R' 'G' = 'B'
combineColors 'R' 'B' = 'G'
combineColors 'B' 'G' = 'R'
combineColors x y = combineColors y x

