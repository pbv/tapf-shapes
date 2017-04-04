-- | Simple library for 2D shapes. Shallow embedding.
module TAPF.Shape.Shallow
  (
  -- * Types
    Shape -- abstract
  -- * Constructor functions
  , empty, disc, square
  -- * Primitive combinators
  , transform, translate
  , union, intersect, invert
  -- * Run functions
  , inside
  ) where

import TAPF.Shape.Matrix

newtype Shape = Shape (Point -> Bool)

-- * Creation functions

empty :: Shape
empty = Shape $ \_ -> False

disc :: Shape
disc = Shape $ \p -> vecX p ^ 2 + vecY p ^ 2 <= 1

square :: Shape
square = Shape $ \p -> abs (vecX p) <= 1 && abs (vecY p) <= 1

-- * Primitive combinators

-- | Transform a shape with a matrix.
transform :: Matrix -> Shape -> Shape
transform m sh = Shape $ \p -> (m' `mul` p) `inside` sh
  where m' = inv m  -- the point is transformed with the inverse matrix

-- | To represent translations as matrix transformations we would need
--   to add another dimension to the matrices (excercise).
translate :: Vec -> Shape -> Shape
translate v sh = Shape $ \p -> inside (p `sub` v) sh

union :: Shape -> Shape -> Shape
union sh1 sh2 = Shape $ \p -> inside p sh1 || inside p sh2

intersect :: Shape -> Shape -> Shape
intersect sh1 sh2 = Shape $ \p -> inside p sh1 && inside p sh2

invert :: Shape -> Shape
invert sh = Shape $ \p -> not (inside p sh)

-- * Run function

inside :: Point -> Shape -> Bool
inside p (Shape f) = f p
