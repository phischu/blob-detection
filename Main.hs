{-# LANGUAGE DeriveFunctor,DeriveFoldable,DeriveTraversable #-}
module Main where

import Codec.Picture.Repa
import Codec.Picture
import Data.Array.Repa hiding ((++),map)
import qualified Data.Array.Repa as Repa
import Numeric.AD
import Numeric.AD.Types
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)



data Blob a = Blob a a a deriving (Show,Functor,Foldable,Traversable)


blob :: Array D DIM2 Double -> Blob Double -> [Blob Double]
blob a b = gradientAscent (blobbiness (toRelation a)) b

blobbiness :: (Mode m) => Relation (AD m Double) -> Blob (AD m Double) -> AD m Double
blobbiness r b = apply (blobmask b) r

toRelation :: (Mode m) => Array D DIM2 Double -> [((AD m Double,AD m Double),AD m Double)]
toRelation arr = Repa.toList (Repa.traverse arr id (\a (Z:.i:.j) -> ((fromIntegral i,fromIntegral j),auto (a (Z:.i:.j)))))

type Relation a = [((a,a),a)]
type Mask a = (a,a) -> a

apply :: (Num a) => Mask a -> Relation a -> a
apply mask image = sum [mask x * v | (x,v) <- image]

blobmask :: (Num a,Floating a,Fractional a) => Blob a -> Mask a
blobmask (Blob x y t) (i,j) = - t * (1/t^4) * (1 / (2*pi)) * (((x-i)^2 + (y-j)^2)/t^2 - 2) * exp ((-1/2) * ((x-i)^2 + (y-j)^2)/t^2)

laplacianOfGaussian :: (Num a,Fractional a,Floating a) => a -> a -> a -> a -> a -> a
laplacianOfGaussian x y t i j = - t * (1/t^4) * (1 / (2*pi)) * (((x-i)^2 + (y-j)^2)/t^2 - 2) * exp ((-1/2) * ((x-i)^2 + (y-j)^2)/t^2)

imageToArray :: Img RGB -> Array D DIM2 Double
imageToArray = Repa.map (/(255*3)) . Repa.sumS . Repa.map fromIntegral . imgData

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
    readImageRGB "squaredirty.png" >>= either print (\sunflowers -> do
        let array = imageToArray sunflowers
            b = take 50 (blob array (Blob 11 12 3))
            pixelArray = doubleArrayToPixel8Array array
            result = renderBlob pixelArray (last b)
            image = arrayToImage result
        putStrLn (unlines (map show b))
        writePng "result.png" image)

renderBlob :: Array D DIM2 Pixel8 -> Blob Double -> Array D DIM2 Pixel8
renderBlob arr b = traverse arr id (\a i -> if i `elem` outline b then 255 else a i)

outline :: Blob Double -> [DIM2]
outline (Blob x y t) =
    let t' = truncate t
        x' = truncate x
        y' = truncate y in
    [Z:.i:.j | let i = y'+t', j <- [x'-t' .. x'+t'] ] ++
    [Z:.i:.j | let i = y'-t', j <- [x'-t' .. x'+t'] ] ++
    [Z:.i:.j | let j = x'+t', i <- [y'-t' .. y'+t'] ] ++
    [Z:.i:.j | let j = x'-t', i <- [y'-t' .. y'+t'] ]








