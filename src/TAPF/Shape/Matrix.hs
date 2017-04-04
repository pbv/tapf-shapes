-- | Simple 2D matrix algebra.
module TAPF.Shape.Matrix
  ( Matrix, Vec, Point, Angle
  , vecX, vecY, ptX, ptY
  , matrix, point, vec
  , mul, inv, sub
  ) where

type Angle  = Double
data Vec    = V { vecX, vecY :: Double }
type Point  = Vec
data Matrix = M Vec Vec

-- | Matrix creation
matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = M (V a b) 
                   (V c d)

-- | Vector creation
vec :: Double -> Double -> Vec
vec = V

-- | Point creation.
point :: Double -> Double -> Point
point = vec

-- | inner product
inner :: Vec -> Vec -> Double
inner (V a b) (V c d) = a * c + b * d

-- | Matrix multiplication
mul :: Matrix -> Vec -> Vec
mul (M r1 r2) v = V (inner r1 v) (inner r2 v)

-- | Matrix inversion
inv :: Matrix -> Matrix
inv (M (V a b) (V c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

-- | Vector Subtraction
sub :: Vec -> Vec -> Vec
sub (V x y) (V dx dy) = V (x - dx) (y - dy)

-- Run functions (projections)

ptX, ptY :: Point -> Double
ptX = vecX
ptY = vecY

