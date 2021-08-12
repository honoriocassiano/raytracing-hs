module Main where

import Data.Function
import Data.List
import GHC.Float.RealFracMethods
import System.IO
import Text.Printf

width = 256
height = 256
filename = "images/image.ppm"

data Pixel = Pixel Float Float Float

instance Show Pixel where
    show (Pixel x y z) = intercalate " " $ map (show . convert) [x, y, z]

-- Integer division resulting in a Float
(/~) :: Integer -> Integer -> Float
(/~) = (/) `on` fromIntegral

infix 7 /~ -- Same precedence of / operator

-- Convert float RGB notation to int RGB notation
convert :: Float -> Integer
convert v = truncateFloatInteger $ v * 255.999

pixel :: Integer -> Integer -> ((Integer, Integer) -> Pixel)
pixel w h =
    (\(x, y) -> Pixel (x /~ (w-1)) (y /~ (h-1)) 0.25)

header :: Integer -> Integer -> String
header w h = printf "P3\n%d %d\n255" w h

printRemaining :: Integer -> IO ()
printRemaining rem = do
    putStr $ printf "\rRemaining lines: %d " rem
    hFlush stdout

scanlines' :: Integer -> Integer -> Integer -> IO [Pixel]
scanlines' 0 _ _ = return []
scanlines' _ 0 _ = return []
scanlines' w h line
    | line < 0  = return []
    | line >= h = return []
    | otherwise = do
        printRemaining(line)
        return $ map (pixel w h) [(x, line) | x <- [0..w-1]]

scanlines :: Integer -> Integer -> IO [[Pixel]]
scanlines w h = mapM (scanlines' w h) $ reverse [0..h-1]

main :: IO ()
main = do
    content <- scanlines width height
    handle  <- openFile filename WriteMode
    hPutStrLn handle $ header width height
    hPutStrLn handle $ intercalate "\n" $ map show $ concat content
    hClose handle
    putStrLn "\nDone."

