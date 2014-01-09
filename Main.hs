module Main where

import Codec.Picture.Repa
import Codec.Picture
import Data.Array.Repa
import Data.Array.Repa as Repa



data Blob a = Blob a a a deriving (Show)


blob :: Array r DIM2 Double -> Blob Double -> Blob Double
blob _ _ = Blob 1 2 3

imageToArray :: Img RGB -> Array D DIM2 Double
imageToArray = Repa.map (/255) . Repa.sumS . Repa.map fromIntegral . imgData

arrayToImage :: Array D DIM2 Double -> Image Pixel8
arrayToImage arr = generateImage (\x y -> pixelArray ! (Z:.y:.x)) w h where
    pixelArray = Repa.map truncate (Repa.map ((*) (255/elementRange)) (Repa.map (+minimumElement) arr))
    minimumElement = foldAllS min (1/0) arr
    maximumElement = foldAllS max (-1/0) arr
    elementRange = maximumElement - minimumElement
    Z:.h:.w = extent arr

main :: IO ()
main = do
    readImageRGB "sunflowers.jpg" >>= either print (\sunflowers -> do
        writePng "result.png" (arrayToImage (imageToArray sunflowers)))







