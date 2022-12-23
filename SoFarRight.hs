module SoFarRight where

import Prelude hiding (foldl, reverse)
import Control.Monad ( (>=>) )

-- foldr _ ini [] = ini
-- foldr f ini (x:xs) = f x (foldr f ini xs)

foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl f ini xs = foldr f' id xs ini where
  f' x k z = k (f z x)

-- foldl (+) 0 [1,2,3] =
--   ~> foldr (\x k z -> k (z + x)) id [1,2,3] 0
--   ~> (\x k z -> k (z + x)) 1 (foldr (\x k z -> k (z + x)) id [2,3]) 0
--   ~> foldr (\x k z -> k (z + x)) id [2,3] (0 + 1)
--   ~> (\x k z -> k (z + x)) 2 (foldr (\x k z -> k (z + x)) id [3]) (0 + 1)
--   ~> foldr (\x k z -> k (z + x)) id [3] ((0 + 1) + 2)
--   ~> (\x k z -> k (z + x)) 3 (foldr (\x k z -> k (z + x)) id []) ((0 + 1) + 2) 
--   ~> foldr (\x k z -> k (z + x)) id [] (((0 + 1) + 2) + 3)
--   ~> id (((0 + 1) + 2) + 3)
--   ~> (((0 + 1) + 2) + 3)

-- foldl (+) 0 [1,2,3] =
--   ~> foldr (\x k z -> k (z + x)) id [1,2,3] 0
--   ~> (\x k z -> k (z + x)) 1 ((\x k z' -> k (z' + x)) 2 ((\x k z'' -> k (z'' + x)) 3 id)) 0
--   ~> (\x k z -> k (z + x)) 1 ((\x k z' -> k (z' + x)) 2 (\z'' -> id (z'' + 3))) 0
--   ~> (\x k z -> k (z + x)) 1 ((\x k z' -> k (z' + x)) 2 (\z'' -> z'' + 3)) 0
--   ~> (\x k z -> k (z + x)) 1 (\z' -> (\z'' -> z'' + 3) (z' + 2)) 0
--   ~> (\x k z -> k (z + x)) 1 (\z' -> (z' + 2) + 3) 0
--   ~> (\z' -> (z' + 2) + 3) (0 + 1)
--   ~> ((0 + 1) + 2) + 3


-- foldl (+) 0 [1,2,3] =
--   ~> foldr (\x k z -> k (z + x)) id [1,2,3] 0
--   ~> (\x k z -> k (z + x)) 1 ((\x k z' -> k (z' + x)) 2 ((\x k z'' -> k (z'' + x)) 3 id)) 0
--   ~> (\x k z' -> k (z' + x)) 2 ((\x k z'' -> k (z'' + x)) 3 id) (0 + 1)
--   ~> (\x k z'' -> k (z'' + x)) 3 id ((0 + 1) + 2)
--   ~> id (((0 + 1) + 2) + 3) 
--   ~> ((0 + 1) + 2) + 3

newtype Update a = Update {evalUpdate :: a -> a}

instance Semigroup (Update a) where
  (Update x) <> (Update y) = Update (y . x)

instance Monoid (Update a) where
  mempty = Update id

foldlMonoid :: (a -> b -> a) -> a -> [b] -> a
foldlMonoid f a bs =
   flip evalUpdate a $
   mconcat $
   map (Update . flip f) bs

foldlMaybe :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlMaybe f a bs =
   foldr (\b g x -> f x b >>= g) Just bs a


newtype UpdateMaybe a = UpdateMaybe {evalUpdateMaybe :: a -> Maybe a}

instance Semigroup (UpdateMaybe a) where
  (UpdateMaybe x) <> (UpdateMaybe y) = UpdateMaybe (x >=> y)

instance Monoid (UpdateMaybe a) where
  mempty = UpdateMaybe Just

foldlMaybeMonoid :: (a -> b -> Maybe a) -> a -> [b] -> Maybe a
foldlMaybeMonoid f a bs =
   flip evalUpdateMaybe a $
   mconcat $
   map (UpdateMaybe . flip f) bs

