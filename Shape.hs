module Shape where

data Shape
  = Square {side :: Double}
  | Rectangle {width :: Double, height :: Double}
  | Triangle {base :: Double, height :: Double}
  | Circle {radius :: Double}
  | CustomShape {area :: Double}
  deriving (Show)

shapeArea :: Shape -> Double
shapeArea Square {side = x} = x ^ 2
shapeArea Rectangle {width = x, height = y} = x * y
shapeArea Triangle {base = x, height = h} = x * h / 2
shapeArea Circle {radius = r} = pi * r ^ 2
shapeArea CustomShape {area = s} = s

instance Eq Shape where
  x == y = shapeArea x == shapeArea y

instance Ord Shape where
  x <= y = shapeArea x <= shapeArea y