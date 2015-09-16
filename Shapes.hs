module Shapes where

import Data.Array.ST
import Data.Array.MArray 
-- oh my goodness, I can't remember the last time I used mutable arrays but here we go
-- I think that using mutable arrays will probably be better than 
import Control.Monad.ST

type Height = Int
type Width = Int
type FPoint = (Float,Float)
type Point = (Int,Int)
type Thickness = Int

type Color = (Int,Int,Int)

{- 
   Here's the preliminary DSL for making the kinds of shapes I want. 
   I'm still arguing with myself on the semantics and it's all a bit silly.

   Ellipses are a bit obvious, with needing to specify their major and minor axes as well as the foci
   
   Arcs are circular arcs with their radius, origin and the radians of the arc specified
   
   Vertical means that the two shapes should be stacked top then bottom

   Horizontal means that they should be left and right next to each other

   On means that the images should be layered within the same space

   Region gives scales for horizontal and vertical relative to the space.

-}

{- 
   Okay, note to self, what we need to do is have a scale of (-1,1) on the x and y-axis for the internal representation of the shapes and then we can map it onto actual regions later with proper scaling. Will this work? I don't know but it seems reasonable to someone who hasn't thought very long about this. 

   Now, you might ask "why have this intermmediate DSL to begin with?" ehh basically because I want to do cute things with random instances and that sort of thing
-}

data Shape = Ellipse Int Int FPoint FPoint Thickness
           | Arc Int FPoint Float Float Thickness 
           | Line FPoint FPoint Thickness -- <-- ^ relative points 
           | Vertical Shape Shape
           | Horizontal Shape Shape
           | On Shape Shape -- layer shapes onto one region
           | Region Float Float Shape -- scaling another shape to fit a region and giving it a background color
           deriving (Eq,Show)

{- 
   Rendering is going to take 
   1. a shape
   2. the actual marray as well as
   3. the current bounds of the region (initially the same as the whole array)

   and of course it'll return the array in the ST monad
-}

{- 
   Let's figure out how lines are going to work:
   Now, let's assume that the FPoints tell us how far to the right and up to set things
   
   The range is going to be (0,0) to (1,1)
   with (0,0) being the bottom left corner, 
        (1,0) is the bottom right corner
        (0,1) is the top left corner
        (1,1) is the top right corner

   if the region for a line covers points (x1, x2, y1, y2) (with x1 < x2, y1 < y2)
   then the regional coordinates are given, as a function of rx and ry (relative coords),
   x = x1 + (x2 - x1)*rx
   y = y1 + (y2 - y1)*ry

   we also have that the slope of the line is going to be given by 
   (fy2 - fy1)/(fx2 - fx1)
   so the set of regional points is going to be given by taking small steps to create an actual
   set of integral points (and then reducing out the duplicates) so how small do we need? 

   Well if we have the equations of the line be
   x = x1 + fx1*(x2 - x1) + (x2 - x1)*(fx2 - fx1)*t
   y = y1 + fy1*(y2 - y1) + (y2 - y1)*(fy2 - fy1)*t

   now I think that's right because if we assume that fx2-fx1 = 0 then
   the x-coordinate will be locked in as x1 + fx1*(x2 - x1)
   if fy2 - fy1 = 0 then the y-coordinate will be locked in as y1 + fy1*(y2 - y1)

   so I think I've got these equations right. Now we just need to figure out what the
   actual fine-grainedness for t should be. It needs to go from 0 to 1 and we could be
   adaptive in the partioning but instead let's just set it at 1000 elements

   so then what we'll have as the code for all of this will be
   
   xs = map $ (round . (\ t -> ...)) $ [0..1000]
   ys = map $ (round . (\ t -> ...)) $ [0..1000]


   Arc's are going to be similar: 
   x = x1 + fx1*(x2 - x1) + r*cos (radd * t + rad1)
   y = y1 + fy1*(y2 - y1) + r*sin (radd * t + rad1)
-}

render :: Shape -> STArray s (Int,Int) Color -> (Int,Int,Int,Int) -> ST s ()
render (Region xscale yscale s) a (x1,x2,y1,y2) = render s a (xmid - xsize, xmid + xsize,
                                                              ymid - ysize, ymid + ysize)
    where xsize = round $ (fromIntegral $ (abs (x2 - x1)) `div` 2) * xscale 
          ysize = round $ (fromIntegral $ (abs (y2 - y1)) `div` 2) * yscale
          xmid = (x1 + x2) `div` 2
          ymid = (y1 + y2) `div` 2
render (Vertical s1 s2) a (x1,x2,y1,y2) = do
  render s1 a (x1,x2, (y1 + y2) `div` 2, y2)
  render s2 a (x1, x2, y1, (y1 + y2) `div` 2)
render (Horizontal s1 s2) a (x1, x2, y1, y2) = do
  render s1 a (x1, (x1 + x2) `div` 2, y1, y2)
  render s2 a ((x1 + x2) `div` 2, x2, y1, y2)
render (On s1 s2) a b = do
  render s1 a b
  render s2 a b
render (Line (fx1, fx2) (fy1, fy2) t) a (x1,x2,y1,y2) = mapM_ (\p -> writeArray a p (0,0,0)) totalps
    where fxd = fx2 - fx1
          fyd = fy2 - fy1
          xd = fromIntegral $ x2 - x1
          yd = fromIntegral $ y2 - y1
          x1' = fromIntegral x1
          x2' = fromIntegral x2
          y1' = fromIntegral y1
          y2' = fromIntegral y2
          xs = map (round . (\s -> x1' + fx1*xd + xd*fxd*(fromIntegral s / 1000))) $ [0..1000]
          ys = map (round . (\s -> y1' + fy1*yd + yd*fyd*(fromIntegral s / 1000))) $ [0..1000]
          ps = zip xs ys
          totalps = concat $ map (\(th1,th2) -> map (\(x,y) -> (x+th1, y+th2)) ps) thicknesses
          thicknesses = do
            x <- [-t..t]
            y <- [-t..t]
            return (x,y)
render (Arc r (fx, fy) rad1 rad2 t) a (x1,x2,y1,y2) = mapM_ (\p -> writeArray a p (0,0,0)) totalps
    where radd = rad2 - rad1
          xd = fromIntegral $ x2 - x1
          yd = fromIntegral $ x2 - x1
          r' = fromIntegral r
          x1' = fromIntegral x1
          x2' = fromIntegral x2
          y1' = fromIntegral y1
          y2' = fromIntegral y2
          xs = map (round . (\s -> x1' + fx*xd + r' * (cos $ radd*(fromIntegral s / 1000) + rad1))) $ [0..1000]
          ys = map (round . (\s -> y1' + fy*yd + r' * (sin $ radd*(fromIntegral s / 1000) + rad1))) $ [0..1000]
          ps = zip xs ys
          totalps = concat $ map (\(th1,th2) -> map (\(x,y) -> (x+th1, y+th2)) ps) thicknesses
          thicknesses = do
            x <- [-t..t]
            y <- [-t..t]
            return (x,y)
render (Ellipse rmajor rminor xc yc t) a p = undefined
