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
           | Arc Int FPoint Float Thickness 
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

render :: Shape -> STArray s (Int,Int) Color -> (Int,Int,Int,Int) -> ST s (STArray s (Int, Int) Color)
render (Region xscale yscale s) a (x1,x2,y1,y2) = render s a (xmid - xsize, xmid + xsize,
                                                              ymid - ysize, ymid + ysize)
    where xsize = round $ (fromIntegral $ (abs (x2 - x1)) `div` 2) * xscale 
          ysize = round $ (fromIntegral $ (abs (y2 - y1)) `div` 2) * yscale
          xmid = (x1 + x2) `div` 2
          ymid = (y1 + y2) `div` 2
render (Vertical s1 s2) a (x1,x2,y1,y2) = do
  a' <- render s1 a (x1,x2, (y1 + y2) `div` 2, y2)
  render s2 a' (x1, x2, y1, (y1 + y2) `div` 2)
render (Horizontal s1 s2) a (x1, x2, y1, y2) = do
  a' <- render s1 a (x1, (x1 + x2) `div` 2, y1, y2)
  render s2 a' ((x1 + x2) `div` 2, x2, y1, y2)
render (On s1 s2) a p = do
  a' <- render s1 a p
  render s2 a' p
