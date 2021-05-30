module Main where

import Control.Monad
import Data.Function
import Data.List
import GHC.Float.RealFracMethods
import Text.Printf

(/~) :: Integer -> Integer -> Float
(/~) = (/) `on` fromIntegral

convert :: Float -> Integer
convert v = truncateFloatInteger $ v * 255.999

generatePixel :: Float -> Float -> [Integer]
generatePixel pi pj = [convert pi, convert pj, convert 0.25]

generateLine :: Integer -> Integer -> ((Integer, Integer) -> String)
generateLine h w =
    (\(i, j) -> do
        let pi    = i /~ h-1
        let pj    = j /~ w-1
        let pixel = map (\p -> show p) $ generatePixel pi pj

        intercalate " " pixel
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

