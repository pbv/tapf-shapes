
module TAPF.Shape.Render where

import TAPF.Shape

data Window = Window
              { bottomLeft :: Point
              , topRight :: Point
              , resolution :: (Int,Int)
              }

pixels :: Window -> [[Point]]
pixels w = [ [point x y
             | x<-take (1+nx) xs]  -- line
           | y<-take (1+ny) ys
           ]  
  where x1 = ptX (bottomLeft w)
        x2 = ptX (topRight w)
        y1 = ptY (bottomLeft w)
        y2 = ptY (topRight w)
        (nx,ny) = resolution w
        dx = (x2-x1)/(fromIntegral nx)        
        dy = (y2-y1)/(fromIntegral ny)
        xs = iterate (+dx) (x1+dx/2)
        ys = iterate (+dy) (y1+dy/2)


pt2pair :: Point -> (Double,Double)
pt2pair p = (ptX p, ptY p)



render :: Window -> Shape -> IO () 
render win sh
  = putStr $ unlines $ map (map pixelAt) (pixels win)
  where pixelAt p | p`inside`sh = 'X'
                  | otherwise   = ' '


-- make a window
window :: Point -> Point -> (Int, Int) -> Window
window = Window

-- default window
window0 = window (point (-1) (-1)) (point 1 1) (20,20)



