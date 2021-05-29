module Main where

import Control.Monad
import Data.List
import GHC.Float.RealFracMethods
import Text.Printf

convert :: Float -> Integer
convert v = truncateFloatInteger $ v * 255.999

pixel :: Float -> Float -> [Integer]
pixel pi pj = [convert pi, convert pj, convert 0.25]

generateHeader :: Integer -> Integer -> [String]
generateHeader height width =
    ["P3", (show width) ++ " " ++ (show height), "255"]

generatePixels :: Integer -> Integer -> [String]
generatePixels height width = map (\(i, j) -> do

    let pi = (fromIntegral i) / (fromIntegral height-1)
    let pj = (fromIntegral j) / (fromIntegral width-1)
    let pix = map (\p -> show p) $ pixel pi pj

    intercalate " " pix) [(i, j) | i <- [0..height-1], j <- [0..width-1]]

generate :: Integer -> Integer -> [String]
generate height width = do
    (generateHeader height width) ++ (generatePixels height width)

main :: IO ()
main = do
    let content = intercalate "\n" $ generate 256 256
    writeFile "image.ppm" content
