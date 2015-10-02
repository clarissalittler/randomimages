module Shapes where

import System.IO.Unsafe

import Data.Array.ST
import Data.Array.MArray 
import Data.Array
-- oh my goodness, I can't remember the last time I used mutable arrays but here we go
-- I think that using mutable arrays will probably be better than 
import Control.Monad.ST
import Control.Monad.Random
import Data.List


type Height = Int
type Width = Int
type FPoint = (Float,Float)
type Point = (Int,Int)
type Thickness = Int

type Color = (Int,Int,Int)

debugPrint a = unsafePerformIO $ print a >> return a

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

data Shape = Arc Float FPoint Float Float Thickness 
           | Line FPoint FPoint Thickness -- <-- ^ relative points 
           | Vertical Shape Shape
           | Horizontal Shape Shape
           | On Shape Shape -- layer shapes onto one region
           | Region Float Float Shape -- scaling another shape to fit a region
           deriving (Eq,Show, Ord)

shapeToInt :: Shape -> Int
shapeToInt (Arc _ _ _ _ _) = 0
shapeToInt (Line _ _ _) = 1
shapeToInt (Vertical _ _) = 2
shapeToInt (Horizontal _ _) = 3
shapeToInt (On _ _) = 4
shapeToInt (Region _ _ _) = 5

instance Random Shape where
    randomR (s1,s2) g = let (i,g') = randomR (shape1, shape2) g
                            shape1 = shapeToInt s1
                            shape2 = shapeToInt s2
                        in case i of
                             0 -> let (radius, g2) = randomR (0,1) g'
                                      (x1, g3) = randomR (0,1) g2
                                      (y1, g4) = randomR (0,1) g3
                                      (rad1, g5) = randomR (0,1) g4
                                      (rad2, g6) = randomR (0,1) g5
                                      (t, g7) = randomR (1,5) g6
                                  in (Arc radius (x1,y1) rad1 rad2 t,g7)
                             1 -> let (x1,g2) = randomR (0,1) g'
                                      (y1,g3) = randomR (0,1) g2
                                      (x2,g4) = randomR (0,1) g3
                                      (y2,g5) = randomR (0,1) g4
                                      (t,g6) = randomR (1,5) g5
                                  in (Line (x1,y1) (x2,y2) t, g6)
                             2 -> let (sh1,g2) = randomR (s1,s2) g'
                                      (sh2,g3) = randomR (s1,s2) g2
                                  in (Vertical sh1 sh2, g3)
                             3 -> let (sh1, g2) = randomR (s1,s2) g'
                                      (sh2, g3) = randomR (s1,s2) g2
                                  in (Horizontal sh1 sh2, g3)
                             4 -> let (sh1, g2) = randomR (s1,s2) g'
                                      (sh2, g3) = randomR (s1,s2) g2
                                  in (On sh1 sh2, g3)
                             5 -> let (x, g2) = randomR (0,1) g'
                                      (y, g3) = randomR (0,1) g2
                                      (sh, g4) = randomR (s1,s2) g3
                                  in (Region x y sh, g4)
                                      
    random g = randomR (Arc undefined undefined undefined undefined undefined,
                        Region undefined undefined undefined) g

randcircle :: RandomGen g => Thickness -> Float -> Float -> Rand g Shape
randcircle t minR maxR = do
  x <- getRandomR (0,1)
  y <- getRandomR (0,1)
  r <- getRandomR (minR, maxR)
  return $ Arc r (x,y) (0 :: Float) (2*pi) t

randarc :: RandomGen g => Thickness -> Float -> Float -> Rand g Shape
randarc t minR maxR = do
  x <- getRandomR (0,1)
  y <- getRandomR (0,1)
  r <- getRandomR (minR, maxR)
  th1 <- getRandomR (0,2*pi)
  th2 <- getRandomR (th1,2*pi)
  return $ Arc r (x,y) th1 th2 t

circle :: Float -> Float -> Float -> Int -> Shape
circle x y r t = Arc r (x,y) 0 (2*pi) t

square :: Float -> Float -> Float -> Int -> Shape
square x y side t = side1 `On` (side2 `On` (side3 `On` side4))
    where side1 = Line (x,y) (x + side, y) t 
          side2 = Line (x + side, y) (x + side, y + side) t
          side3 = Line (x, y) (x, y + side) t
          side4 = Line (x, y + side) (x + side, y + side) t

randSquare :: RandomGen g => Int -> Rand g Shape
randSquare t = do
  x <- getRandomR (0,1)
  y <- getRandomR (0,1)
  s <- getRandomR (0,1)
  return $ square x y s t

triangle :: FPoint -> FPoint -> FPoint -> Thickness -> Shape
triangle p1 p2 p3 t = side1 `On` (side2 `On` side3)
    where side1 = Line p1 p2 t
          side2 = Line p1 p3 t
          side3 = Line p2 p3 t

randPoint :: RandomGen g => Rand g FPoint
randPoint = do
  x <- getRandomR (0,1)
  y <- getRandomR (0,1)
  return (x,y)

randTriangle :: RandomGen g => Thickness -> Rand g Shape
randTriangle t = do
  p1 <- randPoint
  p2 <- randPoint
  p3 <- randPoint
  return $ triangle p1 p2 p3 t

randLine :: RandomGen g => Thickness -> Rand g Shape
randLine t = do
  p1 <- randPoint
  p2 <- randPoint
  return $ Line p1 p2 t

randConnective :: RandomGen g => Rand g (Shape -> Shape -> Shape)
randConnective = do
  c <- getRandomR (1 :: Int,3)
  case c of
    1 -> return On
    2 -> return Vertical
    3 -> return Horizontal
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

safefilter :: [(Int,Int)] -> Int -> Int -> Int -> Int -> [(Int,Int)]
safefilter ps x1 y1 x2 y2 = filter (\(x,y) -> x >= x1 && x <= x2 && y >= y1 && y <= y2) ps

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
render (Line (fx1, fy1) (fx2, fy2) t) a (x1,x2,y1,y2) = mapM_ (\p -> writeArray a p (0,0,0)) totalps
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
          ps = nub $ zip xs ys
          totalps = safefilter (concat $ map (\(th1,th2) -> map (\(x,y) -> (x+th1, y+th2)) ps) thicknesses) x1 y1 x2 y2
          thicknesses = do
            x <- [-(t-1)..(t-1)]
            y <- [-(t-1)..(t-1)]
            return (x,y)
render (Arc r (fx, fy) rad1 rad2 t) a (x1,x2,y1,y2) = mapM_ (\p -> writeArray a p (0,0,0)) totalps
    where radd = rad2 - rad1
          xd = fromIntegral $ x2 - x1
          yd = fromIntegral $ x2 - x1
          r' = r * (sqrt $ xd^2 + yd^2)
          x1' = fromIntegral x1
          x2' = fromIntegral x2
          y1' = fromIntegral y1
          y2' = fromIntegral y2
          xs = map (round . (\s -> x1' + fx*xd + r' * (cos $ radd*(fromIntegral s / 1000) + rad1))) $ [0..1000]
          ys = map (round . (\s -> y1' + fy*yd + r' * (sin $ radd*(fromIntegral s / 1000) + rad1))) $ [0..1000]
          ps = nub $ zip xs ys
          totalps = safefilter (concat $ map (\(th1,th2) -> map (\(x,y) -> (x+th1, y+th2)) ps) thicknesses) x1 y1 x2 y2
          thicknesses = do
            x <- [-(t-1)..(t-1)]
            y <- [-(t-1)..(t-1)]
            return (x,y)

runRender :: Shape -> Int -> Int -> Array (Int,Int) Color
runRender s xsize ysize = runSTArray (runRender' s xsize ysize)

runRender' :: Shape -> Int -> Int -> ST s (STArray s (Int,Int) Color)
runRender' s xsize ysize = do
  a <- newArray ((-xsize `div` 2,-ysize `div` 2),
                 (xsize `div` 2, ysize `div` 2)) (1,1,1)
  render s a (-xsize `div` 2, xsize `div` 2, -ysize `div` 2, ysize `div` 2)
  return a
  
printArray :: (Show a) => Array (Int,Int) a -> String
printArray arr = unlines $ [concat $ row i | i <- [negy..y]]
    where ((negx,negy),(x,y)) = bounds arr
          row i = [ show $ arr ! (rx,i) | rx <- [negx..x] ]

outputArray :: Array (Int,Int) Color -> IO ()
outputArray arr = putStrLn $ printArray $ fmap aux arr
    where aux (0,0,0) = '0'
          aux (1,1,1) = '1'
          
renderTest1 :: IO ()
renderTest1 = do
  let l = Line (0,0) (1,1) 1
      arr = runRender l 10 10
  outputArray arr

renderTest2 :: IO ()
renderTest2 = do
  let l1 = Line (0,0) (1,1) 1
      l2 = Line (0,1) (1,0) 1
      arr = runRender (l1 `On` l2) 10 10
  outputArray arr

renderTest3 :: IO ()
renderTest3 = do
  let l = Line (0,0) (1,1) 1
      arr = runRender (l `Vertical` l) 10 10
  outputArray arr

renderTest4 = do
  let l = Line (0,0) (1,1) 1
      arr = runRender (l `Horizontal` l) 10 10
  outputArray arr

renderTest5 = do
  let t = triangle (0,0) (0.5,1) (1,0) 1
      arr = runRender t 10 10
  outputArray arr

renderTest6 = do
  let s = square 0 0 0.5 1
      arr = runRender s 10 10
  outputArray arr

-- okay this doesn't work, and I don't know why yet
renderTest7 r = do
  let c = circle 0.5 0.5 r 1
      arr = runRender c 20 20
  outputArray arr
