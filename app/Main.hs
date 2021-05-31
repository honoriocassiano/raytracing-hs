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

pixel :: Integer -> Integer -> ((Integer, Integer) -> Pixel)
pixel w h =
    (\(x, y) -> Pixel (x /~ (w-1)) (y /~ (h-1)) 0.25)

header :: Integer -> Integer -> [String]
header w h = ["P3",
              (show w) ++ " " ++ (show h),
              "255"]

pixels :: Integer -> Integer -> [Pixel]
pixels w h = map (pixel w h) [(x, y) | y <- reverse [0..h-1],
                                       x <- [0..w-1]]

generate :: Integer -> Integer -> [String]
generate w h = (header w h) ++ map show (pixels w h)

save :: String -> [String] -> IO ()
save file content = writeFile file (intercalate "\n" content)

main :: IO ()
main = save "image.ppm" $ generate 256 256

