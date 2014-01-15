module Main where

import Codec.Picture.Repa
import Codec.Picture
import Data.Array.Repa hiding ((++))
import qualified Data.Array.Repa as Repa



data Blob a = Blob a a a deriving (Show)


blob :: Array r DIM2 Double -> Blob Double -> Blob Double
blob _ _ = Blob 100 40 33

imageToArray :: Img RGB -> Array D DIM2 Double
imageToArray = Repa.map (/255) . Repa.sumS . Repa.map fromIntegral . imgData

arrayToImage :: Array D DIM2 Pixel8 -> Image Pixel8
arrayToImage arr = generateImage (\x y -> arr ! (Z:.y:.x)) w h where
    Z:.h:.w = extent arr

doubleArrayToPixel8Array :: Array D DIM2 Double -> Array D DIM2 Pixel8
doubleArrayToPixel8Array arr = Repa.map truncate (Repa.map ((*) (255/elementRange)) (Repa.map (+minimumElement) arr)) where
    minimumElement = foldAllS min (1/0) arr
    maximumElement = foldAllS max (-1/0) arr
    elementRange = maximumElement - minimumElement

main :: IO ()
main = do
    readImageRGB "sunflowers.jpg" >>= either print (\sunflowers -> do
        let array = imageToArray sunflowers
            b = blob array (Blob 23 24 25)
            pixelArray = doubleArrayToPixel8Array array
            result = renderBlob pixelArray b
            image = arrayToImage result
        writePng "result.png" image)

renderBlob :: Array D DIM2 Pixel8 -> Blob Double -> Array D DIM2 Pixel8
renderBlob arr blob = traverse arr id (\a i -> if i `elem` outline blob then 255 else a i)

outline :: Blob Double -> [DIM2]
outline (Blob x y t) =
    let t' = truncate (0.5 * t)
        x' = truncate x
        y' = truncate y in
    [Z:.i:.j | let i = y'+t', j <- [x'-t' .. x'+t'] ] ++
    [Z:.i:.j | let i = y'-t', j <- [x'-t' .. x'+t'] ] ++
    [Z:.i:.j | let j = x'+t', i <- [y'-t' .. y'+t'] ] ++
    [Z:.i:.j | let j = x'-t', i <- [y'-t' .. y'+t'] ]








