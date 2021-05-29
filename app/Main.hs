module Main where

import Control.Monad
import GHC.Float.RealFracMethods
import Text.Printf

convert :: Float -> Integer
convert v = truncateFloatInteger $ v * 255.999

pixel :: Float -> Float -> (Integer, Integer, Integer)
pixel pi pj = (convert pi, convert pj, convert 0.25)

generateHeader :: Integer -> Integer -> IO ()
generateHeader height width =
    putStr $ printf "P3\n%d %d\n255\n" width height

generatePixels :: Integer -> Integer -> IO ()
generatePixels height width = mapM_ (\(i, j) -> do
    let pi = (fromIntegral i) / (fromIntegral height-1)
    let pj = (fromIntegral j) / (fromIntegral width-1)
    let (px, py, pz) = pixel pi pj
    putStr $ printf "%d %d %d\n" px py pz) [(i, j) | i <- [0..height-1], j <- [0..width-1]]

main :: IO ()
main = generatePixels 2 2
