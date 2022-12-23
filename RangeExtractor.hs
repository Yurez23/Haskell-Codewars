module RangeExtractor where


solution :: [Integer] -> String
solution = init . helper 

helper [] = ""
helper (x:xs)
    | x == y = show x  ++ "," ++ helper xs
    | x + 1 == y = show x ++ "," ++ show y ++ "," ++ helper ys 
    | otherwise = show x ++ "-" ++ show y  ++ "," ++ helper ys
    where
        (y, ys) = prefixRange (x:xs)

prefixRange [x] = (x, [])
prefixRange (x:y:xs)
    | x + 1 == y = prefixRange (y:xs) 
    | otherwise = (x, y:xs)


test1 = solution [-6,-3,-2,-1,0,1,3,4,5,7,8,9,10,11,14,15,17,18,19,20] == "-6,-3-1,3-5,7-11,14,15,17-20"