module Permutations where

permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)

permutations'            :: [a] -> [[a]]
permutations' xs0        =  xs0 : perms xs0 []

perms :: [a] -> [a] -> [[a]]
perms [] _ = []
perms (t:ts) is = foldr (interleave t ts) (perms ts (t:is)) (permutations' is)

interleave :: a -> [a] -> [a] -> [[a]] -> [[a]]
interleave t ts xs r = let (_,zs) = interleave' t ts id xs r in zs

interleave' :: a -> [a] -> ([a] -> [a]) -> [a] -> [[a]] -> ([a], [[a]])
interleave' _ ts _ [] r = (ts, r)
interleave' t ts f (y:ys) r = let (us,zs) = interleave' t ts (f . (y:)) ys r in (y:us, f (t:y:us) : zs)

-- =
-- permutations [1,2,3]  
--   ~> [1,2,3] : perms [1,2,3] []
--     ~> perms [1,2,3] []  -- t = 1; ts = [2,3]; is = []
--     ~> foldr interleave (perms [2,3] [1]]) (permutations []) -- k = interleave; z = perms [2,3] [1]
--       ~> go (permutations [])
--         ~> permutations [] 
--         ~> [] : perms [] []
--           ~> perms [] []
--           ~> []
--         ~> [] : [] == [[]]
--       ~> go [[]]
--       ~> interleave [] (go [])  -- xs = []; r = go []
--       ~> let (_, zs) = interleave' id [] (go []) in zs
--         ~> interleave' id [] (go [])  -- r = go []
--         ~> ([2,3], go [])
--       ~> let (_, zs) = ([2,3], go []) in zs
--       ~> go []
--     ~> perms [2,3] [1]  -- t = 2; ts = [3]; is = [1]
--     ~> foldr interleave (perms [3] [2,1]) (permutations [1]) -- k = interleave; z = perms [3] [2,1]
--       ~> go (permutations [1])
--         ~> permutations [1]
--         ~> [1] : perms [1] []
--           ~> perms [1] [] -- t = 1; ts = []; is = []
--           ~> foldr interleave (perms [] [1]) (permutations []) -- k = interleave; z = perms [] [1]
--             ~> go (permutations [])
--               ~> permutations []
--               ~> [] : perms [] []
--                 ~> perms [] []
--                 ~> []
--               ~> [] : [] == [[]]
--             ~> go [[]]
--             ~> interleave [] (go [])  -- xs = []; r = go []
--             ~> let (_, zs) = interleave' id [] (go []) in zs
--               ~> interleave' id [] (go [])  -- r = go []
--               ~> ([], go [])
--             ~> let (_, zs) = ([], go []) in zs
--             ~> go []
--           ~> perms [] [1] 
--           ~> []
--         ~> [1] : [] == [[1]]
--       ~> go [[1]]
--       ~> interleave [1] (go [])  -- xs = [1]; r = go []
--       ~> let (_,zs) = interleave' id [1] (go []) in zs
--         ~> interleave' id [1] (go [])  -- f = id; y = 1; ys = []; r = go []
--         ~> let (us,zs) = interleave' (id . (1:)) [] (go []) in (1:us, id (2:1:us) : zs)
--           ~> interleave' (id . (1:)) [] (go [])  -- r = go []
--           ~> ([3], go [])
--         ~> let (us,zs) = ([3], go []) in (1:us, id (2:1:us) : zs)
--         ~> (1:[3], id (2:1:[3]) : go []) == ([1,3], id [2,1,3] : go [])
--       ~> let (_,zs) = ([1,3], id [2,1,3] : go []) in zs
--       ~> id [2,1,3] : go []
--       ~> [2,1,3] : go []
--         ~> go []
--         ~> perms [3] [2,1]
--     ~> [2,1,3] : perms [3] [2,1]
--       ~> perms [3] [2,1]  -- t = 3; ts = []; is = [2,1]
--       ~> foldr interleave (perms [] ([3,2,1])) (permutations [2,1])  -- k = interleave; z = perms [] [3,2,1]
--         ~> go (permutations [2,1])
--           ~> permutations [2,1]
--           ~> [2,1] : perms [2,1] []
--             ~> perms [2,1] []  -- t = 2; ts = [1]; is = []
--             ~> foldr interleave (perms [1] [2]) (permutations [])  -- k = interleave; z = perms [1] [2]
--               ~> go (permutations [])
--                 ~> permutations []
--                 ~> [] : perms [] []
--                   ~> perms [] []
--                   ~> []
--                 ~> [] : [] == [[]]
--               ~> go [[]]
--               ~> interleave [] (go [])  -- xs = []; r = go []
--               ~> let (_,zs) = interleave' id [] (go []) in zs
--                 ~> interleave' id []
--                 ~> ([1], go [])
--               ~> let (_,zs) = ([1], go []) in zs
--               ~> go []
--             ~> perms [1] [2]  -- t = 1; ts = []; is = [2]
--             ~> foldr interleave (perms [] [1,2]) (permutations [2])  -- k = interleave; z = perms [] [1,2]
--               ~> go (permutations [2])
--                 ~> permutations [2]
--                 ~> [2] : perms [2] []
--                   ~> perms [2] []  -- t = 2; ts = []; is = []
--                   ~> foldr interleave (perms [] [2]) (permutations [])  -- k = interleave; z = perms [] [2]
--                     ~> go (permutations [])
--                       ~> permutations []
--                       ~> [] : perms [] []
--                         ~> perms [] []
--                         ~> []
--                       ~> [] : [] == [[]]
--                     ~> go [[]]
--                     ~> interleave [] (go [])  -- xs = []; r = go []
--                     ~> let (_, zs) = interleave' id [] (go []) in zs
--                       ~> interleave' id [] (go [])
--                       ~> ([], go [])
--                     ~> let (_, zs) = ([], go []) (go []) in zs
--                     ~> go []
--                   ~> perms [] [2]
--                   ~> []
--                 ~> [2] : [] == [[2]]
--               ~> go [[2]]
--               ~> interleave [2] (go []) -- xs = [2]; r = go []
--               ~> let (_,zs) = interleave' id [2] (go []) in zs
--                 ~> interleave' id [2] (go [])  -- f = id; y = 2; ys = []; r = go []
--                 ~> let (us,zs) = interleave' (id . (2:)) [] (go []) in (2:us, id (1:2:us) : zs)
--                   ~> interleave' (id . (2:)) [] (go [])
--                   ~> ([], go [])
--                 ~> let (us,zs) = ([], go []) in (2:us, id (1:2:us) : zs)
--                 ~> (2:[], id (1:2:[]) : go []) == ([2], id [1,2] : go [])
--               ~> let (_,zs) = ([2], id [1,2] : go []) in zs
--               ~> id [1,2] : go []
--               ~> [1,2] : go []
--                 ~> go []
--                 ~> perms [] [1,2]
--             ~> [1,2] : perms [] [1,2]
--               ~> perms [] [1,2]
--               ~> []
--             ~> [1,2] : [] == [[1,2]]
--           ~> [2,1] : [[1,2]] = [[2,1],[1,2]]
--         ~> go [[2,1],[1,2]]
--         ~> interleave [2,1] (go [[1,2]])  -- xs = [2,1]; r = go [[1,2]]
--         ~> let (_,zs) = interleave' id [2,1] (go [[1,2]]) in zs
--           ~> interleave' id [2,1] (go [[1,2]])  -- f = id; y = 2; ys = [1]; r = go [[1,2]]
--           ~> let (us,zs) = interleave' (id . (2:)) [1] (go [[1,2]]) in (2:us, id (3:2:us) : zs)
--             ~> interleave' (id . (2:)) [1] (go [[1,2]])  -- f = id . (2:); y = 1; ys = []; r = go [[1,2]]
--             ~> let (us,zs) = interleave' (id . (2:) . (1:)) [] (go [[1,2]]) in (1:us, (id . (2:)) (3:1:us) : zs)
--               ~> interleave' (id . (2:) . (1:)) [] (go [[1,2]])
--               ~> ([], go [[1,2]])
--             ~> let (us,zs) = ([], go [[1,2]]) in (1:us, (id . (2:)) (3:1:us) : zs)
--             ~> (1:[], (id . (2:)) (3:1:[]) : go [[1,2]]) == ([1], (id . (2:)) [3,1] : go [[1,2]])
--           ~> let (us,zs) = ([1], (id . (2:)) [3,1] : go [[1,2]]) in (2:us, id (3:2:us) : zs)
--           ~> (2:[1], id (3:2:[1]) : (id . (2:)) [3,1] : go [[1,2]]) == ([2,1], id [3,2,1] : (id . (2:)) [3,1] : go [[1,2]])
--         ~> let (_,zs) = ([2,1], id [3,2,1] : (id . (2:)) [3,1] : go [[1,2]]) in zs
--         ~> id [3,2,1] : (id . (2:)) [3,1] : go [[1,2]]
--         ~> [3,2,1] : (id . (2:)) [3,1] : go [[1,2]]
--         ~> [3,2,1] : [2,3,1] : go [[1,2]]
--           ~> go [[1,2]]
--           ~> interleave [1,2] (go [])  -- xs = [1,2]; r = go []
--           ~> let (_,zs) = interleave' id [1,2] (go []) in zs
--             ~> interleave' id [1,2] (go [])  -- f = id; y = 1; ys = [2]; r = go []
--             ~> let (us,zs) = interleave' (id . (1:)) [2] (go []) in (1:us, id (3:1:us) : zs)
--               ~> interleave' (id . (1:)) [2] (go [])  -- f = id . (1:); y = 2; ys = []; r = go []
--               ~> let (us,zs) = interleave' ((id . (1:)) . (2:)) [] (go []) in (2:us, (id . (1:)) (3:2:us) : zs)
--                 ~> interleave' ((id . (1:)) . (2:)) [] (go []) 
--                 ~> ([], go [])
--               ~> let (us,zs) = ([], go []) in (2:us, (id . (1:)) (3:2:us) : zs)
--               ~> (2:[], (id . (1:)) (3:2:[]) : go []) == ([2], (id . (1:)) [3,2] : go [])
--             ~> let (us,zs) = ([2], (id . (1:)) [3,2] : go []) in (1:us, id (3:1:us) : zs)
--             ~> (1:[2], id (3:1:[2]) : (id . (1:)) [3,2] : go []) == ([1,2], id [3,1,2] : (id . (1:)) [3,2] : go [])
--           ~> let (_,zs) = ([1,2], id [3,1,2] : (id . (1:)) ([3,2]) : go []) in zs
--           ~> id [3,1,2] : (id . (1:)) [3,2] : go []
--           ~> [3,1,2] : (id . (1:)) [3,2] : go []
--           ~> [3,1,2] : [1,3,2] : go []
--             ~> go []
--             ~> perms [] [3,2,1]
--           ~> [3,1,2] : [1,3,2] : perms [] [3,2,1]
--             ~> perms [] [3,2,1]
--             ~> []
--           ~> [3,1,2] : [1,3,2] : [] 
--         ~> [3,2,1] : [2,3,1] : [3,1,2] : [1,3,2] : []
--     ~> [2,1,3] : [3,2,1] : [2,3,1] : [3,1,2] : [1,3,2] : []
--   ~> [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]

-- permutations [] =
--   ~> [] : perms [] []
--   ~> [] : [] == [[]]

-- permutations [x] = 
--   ~> [x] : perms [x] []
--     ~> perms [x] []
--     ~> perms [] [x]
--     ~> []
--   ~> [x] : [] = [[x]] 

-- perms (t:ts) [] = 
--   ~> foldr interleave (perms ts [t]) (permutations [])
--   ~> foldr interleave (perms ts [t]) [[]]
--   ~> interleave [] (perms ts [t])
--   ~> perms ts [t]

-- perms (t:ts) [x] =
--   ~> foldr interleave (perms ts [t,x]) (permutations [x])
--   ~> foldr interleave (perms ts [t,x]) [[x]]
--   ~> interleave [x] (perms ts [t,x])
--   ~> (t:x:ts) : perms ts [t,x]

-- interleave [] r =
--   ~> let (_,zs) = interleave' id [] r in zs
--     ~> interleave' id [] r
--     ~> (ts, r)
--   ~> let (_,zs) = (ts, r) in zs
--   ~> r

-- interleave [y] r =
--   ~> let (_,zs) = interleave' id [y] r in zs
--     ~> interleave' id [y] r
--     ~> let (us,zs) = interleave' (id . (y:)) [] r in (y:us, id (t:y:us) : zs)
--       ~> interleave' (id . (y:)) [] r
--       ~> (ts, r)
--     ~> let (us,zs) = (ts, r) in (y:us, id (t:y:us) : zs)
--     ~> (y:ts, id (t:y:ts) : r)
--   ~> let (_,zs) = (y:ts, id (t:y:ts) : r) in zs
--   ~> id (t:y:ts) : r
--   ~> (t:y:ts) : r

-- interleave [x,y] r =
--   ~> let (_,zs) = interleave' id [x,y] r in zs
--     ~> interleave' id [x,y] r
--     ~> let (us,zs) = interleave' (id . (x:)) [y] r in (x:us, id (t:x:us) : zs)
--       ~> interleave' (id . (x:)) [y] r
--       ~> let (us,zs) = interleave' ((id . (x:)) . (y:)) [] r in (y:us, (id . (x:)) (t:y:us) : zs)
--         ~> interleave' ((id . (x:)) . (y:)) [] r
--         ~> (ts, r)
--       ~> let (us,zs) = (ts, r) in (y:us, (id . (x:)) (t:y:us) : zs)
--       ~> (y:ts, (id . (x:)) (t:y:ts) : r)
--     ~> let (us,zs) = (y:ts, (id . (x:)) (t:y:ts) : r) in (x:us, id (t:x:us) : zs)
--     ~> (x:y:ts, id (t:x:y:ts) : (id . (x:)) (t:y:ts) : r)
--   ~> let (_,zs) = (x:y:ts, id (t:x:y:ts) : (id . (x:)) (t:y:ts) : r) in zs
--   ~> id (t:x:y:ts) : (id . (x:)) (t:y:ts) : r
--   ~> (t:x:y:ts) : (id . (x:)) (t:y:ts) : r
--   ~> (t:x:y:ts) : (x:t:y:ts) : r



-- permutations [1,2,3] =
--   ~> [1,2,3] : perms [1,2,3] []
--     ~> perms [1,2,3] [] 
--     ~> perms [2,3] [1]  -- t = 2; ts = [3]; is = [1]
--     ~> foldr interleave (perms [3] [2,1]) (permutations [1])
--     ~> foldr interleave (perms [3] [2,1]) [[1]]
--     ~> interleave [1] (perms [3] [2,1])  
--     ~> [2,1,3] : perms [3] [2,1]
--       ~> perms [3] [2,1]  -- t = 3; ts = []; is = [2,1]
--       ~> foldr interleave (perms [] [3,2,1]) (permutations [2,1])
--         ~> permutations [2,1]
--         ~> [2,1] : perms [2,1] []
--           ~> perms [2,1] []  
--           ~> perms [1] [2]  -- t = 1; ts = []; is = [2]
--           ~> foldr interleave (perms [] [1,2]) (permutations [2])
--           ~> foldr interleave [] [[2]]
--           ~> interleave [2] []
--           ~> [1,2] : []
--         ~> [[2,1],[1,2]]
--       ~> foldr interleave [] [[2,1],[1,2]]
--       ~> interleave [2,1] (interleave [1,2] [])
--       ~> [3,2,1] : [2,3,1] : interleave [1,2] []
--         ~> interleave [1,2] []
--         ~> [3,1,2] : [1,3,2] : []
--       ~> [3,2,1] : [2,3,1] : [3,1,2] : [1,3,2] : []
--     ~> [2,1,3] : [3,2,1] : [2,3,1] : [3,1,2] : [1,3,2] : []
--   ~> [1,2,3] : [2,1,3] : [3,2,1] : [2,3,1] : [3,1,2] : [1,3,2] : []

-- =
-- permutations [1,2,3]
--   ~> [1,2,3] : perms [1,2,3] []
--     ~> perms [1,2,3] []
--     ~> foldr (interleave 1 [2,3]) (perms [2,3] [1]) (permutations [])
--     ~> foldr (interleave 1 [2,3]) (perms [2,3] [1]) [[]]
--     ~> interleave 1 [2,3] [] (perms [2,3] [1])
--       ~> perms [2,3] [1]
--       ~> foldr (interleave 2 [3]) (perms [3] [2,1]) (permutations [1])
--       ~> foldr (interleave 2 [3]) (perms [3] [2,1]) [[1]]
--       ~> interleave 2 [3] [1] (perms [3] [2,1])
--         ~> perms [3] [2,1]
--         ~> foldr (interleave 3 []) (perms [] [3,2,1]) (permutations [2,1])
--           ~> permutations [2,1]
--           ~> [2,1] : perms [2,1] []
--             ~> perms [2,1] []
--             ~> foldr (interleave 2 [1]) (perms [1] [2]) [[]]
--             ~> interleave 2 [1] [] (perms [1] [2])
--               ~> perms [1] [2]
--               ~> foldr (interleave 1 []) (perms [] [1,2]) [[2]]
--               ~> interleave 1 [] [2] (perms [] [1,2])
--               ~> interleave 1 [] [2] []
--             ~> interleave 2 [1] [] (interleave 1 [] [2] [])
--           ~> [2,1] : interleave 2 [1] [] (interleave 1 [] [2] [])
--           ~> [2,1] : interleave 1 [] [2] []
--           ~> [2,1] : [[1,2]] == [[2,1],[1,2]]
--         ~> foldr (interleave 3 []) (perms [] [3,2,1]) [[2,1],[1,2]]
--         ~> interleave 3 [] [2,1] (interleave 3 [] [1,2] (perms [] [3,2,1]))
--         ~> interleave 3 [] [2,1] (interleave 3 [] [1,2] [])
--       ~> interleave 2 [3] [1] (interleave 3 [] [2,1] (interleave 3 [] [1,2] []))
--     ~> interleave 1 [2,3] [] (interleave 2 [3] [1] (interleave 3 [] [2,1] (interleave 3 [] [1,2] [])))
--   ~> [1,2,3] : interleave 1 [2,3] [] (interleave 2 [3] [1] (interleave 3 [] [2,1] (interleave 3 [] [1,2] [])))
    
{-
>>> [1,2,3] : interleave 1 [2,3] [] (interleave 2 [3] [1] (interleave 3 [] [2,1] (interleave 3 [] [1,2] [])))
[[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
-}

        
-- =        
-- permutations [2,1]
--   ~> [2,1] : perms [2,1] []
--     ~> perms [2,1] []
--     ~> foldr (interleave 2 [1]) (perms [1] [2]) [[]]
--     ~> interleave 2 [1] [] (perms [1] [2])
--       ~> perms [1] [2]
--       ~> foldr (interleave 1 []) (perms [] [1,2]) [[2]]
--       ~> interleave 1 [] [2] (perms [] [1,2])
--       ~> interleave 1 [] [2] []
--     ~> interleave 2 [1] [] (interleave 1 [] [2] [])
--   ~> [2,1] : interleave 2 [1] [] (interleave 1 [] [2] [])


-- permutations            :: [a] -> [[a]]
-- permutations xs0        =  xs0 : perms xs0 []
--   where
--     perms []     _  = []
--     perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
--       where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
--             interleave' _ []     r = (ts, r)
--             interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
--                                      in  (y:us, f (t:y:us) : zs)
