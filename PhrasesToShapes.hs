module PhrasesToShapes where

import Shapes
import ShapeToBmp

import Control.Monad.Random
import Control.Monad

import Codec.Picture

{- 
   This part is going to be somewhat arbitrary but I'll base these off of rough shapes of letters and we can go from there.

-}

charToShape :: RandomGen g => Char -> Rand g Shape
charToShape 'A' = do
  t <- getRandomR (0.05,0.1)
  tri1 <- randTriangle t
  tri2 <- randTriangle t
  conn <- randConnective
  return $ conn tri1 tri2
charToShape 'B' = do
  t <- getRandomR (0.05,0.1)
  l <- randLine t
  c1 <- randcircle t 0.1 0.9
  c2 <- randcircle t 0.1 0.9
  conn1 <- randConnective
  conn2 <- randConnective
  return $ conn1 l (conn2 c1 c2)
charToShape 'C' = do
  t <- getRandomR (0.05,0.1)
  randarc t 0.1 0.9
charToShape 'D' = do
  t <- getRandomR (0.05,0.1)
  l <- randLine t
  r <- randarc t 0.1 0.9
  conn <- randConnective
  return $ conn l r
charToShape 'E' = do
  t <- getRandomR (0.05,0.1)
  l1 <- randLine t
  l2 <- randLine t
  l3 <- randLine t
  return $ mirrorPairV (l1 `On` (l2 `On` l3))
charToShape 'F' = do
  t <- getRandomR (0.05, 0.1)
  l1 <- randLine t
  l2 <- randLine t
  conn <- randConnective
  return $ conn l1 l2 
charToShape 'G' = do
  t <- getRandomR (0.05, 0.1)
  a <- randarc t 0.05 0.5
  l <- randLine t
  conn <- randConnective
  return $ conn l a
charToShape 'H' = do
  s <- getRandom
  return $ mirrorPairH s
charToShape 'I' = do
  t <- getRandomR (0.05, 0.1)
  l <- randLine t
  return $ mirrorPairV l
charToShape 'J' = do
  t <- getRandomR (0.05, 0.2)
  l <- randLine t
  a <- randarc t 0.05 0.5
  return $ l `Vertical` a
charToShape 'K' = do
  l1 <- randLine 0.05
  l2 <- randLine 0.05
  return $ mirrorPairV (l1 `Horizontal` l2)
charToShape 'L' = do
  t <- getRandomR (0.05,0.1)
  l1 <- randLine t
  l2 <- randLine t
  return $ Vertical l1 l2
charToShape 'M' = charToShape 'W'
charToShape 'N' = charToShape 'Z'
charToShape 'O' = randcircle 0.05 0.1 0.5
charToShape 'P' = do
  t <- getRandomR (0.05, 0.2)
  l <- randLine t
  c <- randcircle t 0.1 0.5
  return $ l `On` c
charToShape 'Q' = do
  c <- randcircle 0.05 0.1 0.5
  l <- randLine 0.05
  return $ Vertical c l
charToShape 'R' = do
  t <- getRandomR (0.05, 0.1)
  a <- randarc t 0.05 0.1
  l1 <- randLine t
  l2 <- randLine t
  return $ a `Vertical` (l1 `Horizontal` l2)
charToShape 'S' = do
  a <- randarc 0.05 0.1 0.5
  return $ a `Vertical` (mirrorH a)
charToShape 'T' = do
  t <- getRandomR (0.05, 0.1)
  l1 <- randLine t
  l2 <- randLine t
  return $ Vertical (mirrorPairH l1) l2
charToShape 'U' = randarc 0.05 0.05 0.1
charToShape 'V' = do
  t <- getRandomR (0.05, 0.1)
  l <- randLine t
  return $ mirrorPairH l
charToShape 'W' = do
  t <- getRandomR (0.05, 0.1)
  l1 <- randLine t
  l2 <- randLine t
  return $ mirrorPairH (l1 `Horizontal` l2)
charToShape 'X' = do
  t <- getRandomR (0.05, 0.1)
  l <- randLine t
  return $ l `On` (mirrorH l)
charToShape 'Y' = do
  t <- getRandomR (0.05, 0.1)
  l1 <- randLine t
  l2 <- randLine t
  l3 <- randLine t
  return $ (l1 `Horizontal` l2) `Vertical` l3
charToShape 'Z' = do
  t <- getRandomR (0.05, 0.06)
  l1 <- randLine t
  l2 <- randLine t
  return $ mirrorPairV (mirrorPairH $ l1 `On` l2)

phraseToShape :: (RandomGen g) => String -> Rand g Shape
phraseToShape = foldM aux Empty
 
aux :: (RandomGen g) => Shape -> Char -> Rand g Shape   
aux s c = do
  x <- getRandomR (0,1 :: Int)
  case x of
    0 -> do
      s' <- charToShape c
      return $ s `On` s'
    1 -> return s

stringToImage :: String -> String -> IO ()
stringToImage path phrase = do
  s <- evalRandIO $ phraseToShape phrase
  saveBmpImage path $ ImageRGB16 $ shapeToImage s 500 500
