{-# LANGUAGE FlexibleContexts #-}
import Data.List.Split
import Data.List
import Data.Function
import Debug.Trace

type Vec = [Int]
type Matrix3 = [Vec]

parsePoint :: String -> Vec
parsePoint = map read . splitOn ","

prepare = map (map parsePoint . tail . lines) . splitOn "\n\n"

vecProd v1 v2 = sum $ zipWith (*) (map fromIntegral v1) v2

matrixProd m v = map (round . vecProd v) (transpose m)

rotateX theta = matrixProd [[1,0,0], [0, cos theta, sin theta], [0, -sin theta, cos theta]]
rotateY theta = matrixProd [[cos theta, 0, -sin theta], [0,1,0], [sin theta, 0, cos theta]]
rotateZ theta = matrixProd [[cos theta, sin theta, 0], [-sin theta, cos theta, 0], [0,0,1]]

orientations :: Vec -> [Vec]
orientations p = [rotateZ theta x | x <- orients, theta <- thetas]
    where thetas = [0,pi/2.0,pi,3.0*pi/2]
          orients = map (`rotateY` p) thetas ++ [rotateX (pi/2) p, rotateX (-pi/2) p]

distance a b = sqrt . sum . map (**2) $ zipWith (-) (map fromIntegral a) (map fromIntegral b)
magnitude = distance (repeat 0)

translateBy = zipWith (-)
translateAllBy vec = map (translateBy vec)

--solve1 [] = [[]]
solve1 (x:xs) = zipWith (zipWith (-)) (translateAllBy z x) (translateAllBy y (head xs))
    where y = minimumBy (compare `on` magnitude) $ head xs
          z = minimumBy (compare `on` magnitude) x

solve2 = id

main = do
    file <- readFile "test_input.txt"
    --file <- readFile "input.txt"
    putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
    --putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)
