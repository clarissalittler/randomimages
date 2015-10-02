module ShapeToBmp where

import Shapes
import Codec.Picture -- from JuicyPixels
import Data.Array

maxPixel = 65535

convert :: Array (Int,Int) Color -> Image PixelRGB16
convert a = generateImage aux width height
    where width = 2 * (fst $ snd $ bounds a)
          height = 2 * (snd $ snd $ bounds a)
          aux x y = convertColor $ a ! (transx x, transy y)
          convertColor (1,1,1) = PixelRGB16 maxPixel maxPixel maxPixel
          convertColor (x,y,z) = PixelRGB16 0 0 0  -- yes, this is silly but it works until I decide if I want real color
          transx x = x - width `div` 2
          transy y = (height `div` 2) -y

shapeToImage :: Shape -> Int -> Int -> Image PixelRGB16
shapeToImage s x y = convert $ runRender s x y

imageTest :: IO ()
imageTest = do
  let l = Line (0,0) (1,1) 5
      arr = runRender l 500 500
      img = ImageRGB16 $ convert arr
  saveBmpImage "test1.bmp" img
