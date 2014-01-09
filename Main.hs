module Main where

import Codec.Picture
import Codec.Picture.Types
import Data.Array.Repa
import Data.Array.Repa as Repa
import Data.Array.Repa.Repr.Vector as Repa
import Data.Vector as Vector



data Blob a = Blob a a a deriving (Show)


blob :: Array r DIM2 Double -> Blob Double -> Blob Double
blob _ _ = Blob 1 2 3

loadGrayImage :: FilePath -> IO (Either String (Array D DIM2 Double))
loadGrayImage filepath = do
	eitherdynamicimage <- readImage filepath
	return (eitherdynamicimage >>= extractLuma >>= return . imageToArray >>= return . (Repa.map pixel8ToDouble))

extractLuma :: DynamicImage -> Either String (Image Pixel8)
extractLuma (ImageRGB8 image) = Right (extractLumaPlane image)
extractLuma (ImageRGBA8 image) = Right (extractLumaPlane image)
extractLuma (ImageYCbCr8 image) = Right (extractLumaPlane image)
extractLuma _ = Left "Image not RGB8 or RGBA8 or YCbCr8"

imageToArray :: Image Pixel8 -> Array V DIM2 Pixel8
imageToArray (Image w h d) = Repa.fromVector (Z:.w:.h) (Vector.convert d)

pixel8ToDouble :: Pixel8 -> Double
pixel8ToDouble pixel = fromIntegral pixel / 255

main :: IO ()
main = do
	loadGrayImage "sunflowers.jpg" >>= either print (\sunflowers -> do
		print (blob sunflowers (Blob 12 45 5))) 







