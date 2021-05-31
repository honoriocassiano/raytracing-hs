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
pixel h w =
    (\(i, j) -> Pixel (i /~ (w-1)) (j /~ (h-1)) 0.25)

header :: Integer -> Integer -> [String]
header h w = ["P3",
              (show w) ++ " " ++ (show h),
              "255"]

pixels :: Integer -> Integer -> [Pixel]
pixels h w = map (pixel h w) [(i, j) | j <- reverse [0..h-1],
                                       i <- [0..w-1]]

generate :: Integer -> Integer -> [String]
generate h w = (header h w) ++ map show (pixels h w)

save :: String -> [String] -> IO ()
save file content = writeFile file (intercalate "\n" content)

main :: IO ()
main = save "image.ppm" $ generate 256 256

