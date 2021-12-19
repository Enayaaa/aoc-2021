import Data.List.Split
import Data.List

type Vec = [Int]
type Matrix3 = [Vec]

parsePoint :: String -> Vec
parsePoint = map read . splitOn ","

prepare = map (map parsePoint . tail . lines) . splitOn "\n\n"

vecProd v1 v2 = sum $ zipWith (*) v1 v2

matrixProd m v = map (round . vecProd v) (transpose m)

rotateY theta = matrixProd [[cos theta, 0, -sin theta], [0,1,0], [sin theta, 0, cos theta]]
rotateZ theta = matrixProd [[cos theta, sin theta, 0], [-sin theta, cos theta, 0], [0,0,1]]

orientations (x,y,z) = []

solve1 = id

solve2 = id

main = do
    file <- readFile "test_input.txt"
    --file <- readFile "input.txt"
    putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
    --putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)
