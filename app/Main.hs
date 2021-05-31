module Main where

import Data.Function
import Data.List
import GHC.Float.RealFracMethods

data Pixel = Pixel Float Float Float

instance Show Pixel where
    show (Pixel x y z) = intercalate " " $ map (show . convert) [x, y, z]

-- Integer division resulting in a Float
(/~) :: Integer -> Integer -> Float
(/~) = (/) `on` fromIntegral

infix 7 /~ -- Same precedence of / operator

-- Convert float RGB notation to int RGB notation
convert :: Float -> Integer
convert v = truncateFloatInteger (v * 255.999)

generatePixel :: Integer -> Integer -> ((Integer, Integer) -> Pixel)
generatePixel h w =
    (\(i, j) -> Pixel (i /~ (w-1)) (j /~ (h-1)) 0.25)

generateHeader :: Integer -> Integer -> [String]
generateHeader h w = ["P3",
                      (show w) ++ " " ++ (show h),
                      "255"]

generatePixels :: Integer -> Integer -> [Pixel]
generatePixels h w = map (generatePixel h w) [(i, j) | j <- reverse [0..h-1],
                                                       i <- [0..w-1]]

generate :: Integer -> Integer -> [String]
generate h w = (generateHeader h w) ++ map show (generatePixels h w)

main :: IO ()
main = do
    -- let content = intercalate "\n" $ generate 256 256
    writeFile "image.ppm" $ intercalate "\n" $ generate 256 256

