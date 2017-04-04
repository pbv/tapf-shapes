{-
  Case study: a Haskell DSEL for shapes

  Pedro Vasconcelos, 2015
  Based on code by Patrik Janson:
  https://github.com/AFPChalmers15/AFPcourse/tree/master/L2
-}
module TAPF.Shape
   ( module Impl
   , module TAPF.Shape
   , module TAPF.Shape.Matrix
   )
   where

import TAPF.Shape.Matrix
-- shallow embedding implementation
import TAPF.Shape.Shallow as Impl

-- | Derived operations
scale :: Vec -> Shape -> Shape
scale v = transform (matrix  (vecX v)  0 
                             0         (vecY v))

rotate :: Angle -> Shape -> Shape
rotate a = transform (matrix  c  (-s) 
                              s  c   )
  where  c = cos a
         s = sin a

difference :: Shape -> Shape -> Shape
difference sh1 sh2 = sh1 `intersect` invert sh2
