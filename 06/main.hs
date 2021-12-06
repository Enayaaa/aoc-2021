import Data.List.Split

-- Struggle day, overcomplicated alot before I got help with this

count e = length . filter (==e)

prepare :: String -> [Int]
prepare = map read . splitOn ","

step fs = let f = head fs in zipWith (+) (tail fs ++ [0]) $ replicate 6 0 ++ [f,0,f]

getEndLength n fs
    = sum 
    $ iterate step (map (\x -> length $ filter (==x) fs) [0..8]) !! n

solve1 = getEndLength 80

solve2 = getEndLength 256

main :: IO ()
main = do
    file <- readFile "input.txt"
    putStrLn $ "part 1: "++(show . solve1 . prepare $ file)
    putStrLn $ "part 2: "++(show . solve2 . prepare $ file)

