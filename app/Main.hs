module Main where

import Data.Function
import Data.List
import GHC.Float.RealFracMethods

data Pixel = Pixel Float Float Float

instance Show Pixel where
    show (Pixel x y z) = intercalate " " (map (show . convert) [x, y, z])

-- Integer division resulting in a Float
(/~) :: Integer -> Integer -> Float
(/~) = (/) `on` fromIntegral

convert :: Float -> Integer
convert v = truncateFloatInteger $ v * 255.999

generatePixel :: Float -> Float -> Pixel
generatePixel pi pj = Pixel pi pj 0.25

generateLine :: Integer -> Integer -> ((Integer, Integer) -> String)
generateLine h w =
    (\(i, j) -> do
        let pi    = i /~ (h-1)
        let pj    = j /~ (w-1)
        let pixel = generatePixel pi pj

        show pixel
    )

generateHeader :: Integer -> Integer -> [String]
generateHeader h w = ["P3",
                      (show w) ++ " " ++ (show h),
                      "255"]

generatePixels :: Integer -> Integer -> [String]
generatePixels h w = map (generateLine h w) [(i, j) | i <- [0..h-1],
                                                      j <- [0..w-1]]

generate :: Integer -> Integer -> [String]
generate h w = (generateHeader h w) ++ (generatePixels h w)

main :: IO ()
main = do
    let content = intercalate "\n" $ generate 256 256
    writeFile "image.ppm" content

