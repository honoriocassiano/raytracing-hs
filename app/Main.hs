module Main where

import Control.Monad
import GHC.Float.RealFracMethods
import Text.Printf

convert :: Float -> Integer
convert v = truncateFloatInteger $ v * 255.999

pixel :: Float -> Float -> (Integer, Integer, Integer)
pixel pi pj = (convert pi, convert pj, convert 0.25)

generate :: Integer -> Integer -> IO ()
generate height width = mapM_ (\(i, j) -> do
    let pi = (fromIntegral i) / (fromIntegral height-1)
    let pj = (fromIntegral j) / (fromIntegral width-1)
    let (px, py, pz) = pixel pi pj
    putStrLn $ printf "%d %d %d" px py pz) [(i, j) | i <- [0..height-1], j <- [0..width-1]]

main :: IO ()
main = generate 2 2
