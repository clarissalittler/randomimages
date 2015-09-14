module Shapes where

import Data.Array.ST
import Data.Array.MArray 
-- oh my goodness, I can't remember the last time I used mutable arrays but here we go
-- I think that using mutable arrays will probably be better than 
import Control.Monad.ST

type Height = Int
type Width = Int
type Point = (Int,Int)

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

data Shape = Ellipse Int Int Point Point Thickness
           | Arc Int Point Float Thickness 
           | Line Point Point Thickness -- <-- ^ relative points 
           | Vertical Shape Shape
           | Horizontal Shape Shape
           | On Shape Shape -- layer shapes onto one region
           | Region Float Float Shape -- scaling another shape to fit a region and giving it a background color
           deriving (Eq,Show)

