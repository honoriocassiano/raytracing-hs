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
convert v = truncateFloatInteger $ v * 255.999

pixel :: Integer -> Integer -> ((Integer, Integer) -> Pixel)
pixel w h =
    (\(x, y) -> Pixel (x /~ (w-1)) (y /~ (h-1)) 0.25)

header :: Integer -> Integer -> [String]
header w h = ["P3",
              show w ++ " " ++ show h,
              "255"]

pixels :: Integer -> Integer -> [Pixel]
pixels w h = map (pixel w h) [(x, y) | y <- reverse [0..h-1],
                                       x <- [0..w-1]]

save :: String -> [String] -> [Pixel] -> IO ()
save file header content =
    writeFile file $ intercalate "\n" $ header ++ (map show content)

main :: IO ()
main = do
    let (w, h) = (256, 256)
    let filename = "image.ppm"

    let head = header w h
    let content = pixels w h

    save filename head content

