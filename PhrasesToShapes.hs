module PhrasesToShapes where

import Shapes

import Control.Monad.Random
import Control.Monad

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
  return $ mirroredPairV (l1 `On` (l2 `On` l3))
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
  return $ mirroredPairH s
charToShape 'I' = undefined
charToShape 'J' = undefined
charToShape 'K' = undefined
charToShape 'L' = undefined
charToShape 'M' = undefined
charToShape 'N' = undefined
charToShape 'O' = undefined
charToShape 'P' = undefined
charToShape 'Q' = undefined
charToShape 'R' = undefined
charToShape 'S' = undefined
charToShape 'T' = undefined
charToShape 'U' = undefined
charToShape 'V' = undefined
charToShape 'W' = undefined
charToShape 'X' = undefined
charToShape 'Y' = undefined
charToShape 'Z' = undefined

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
