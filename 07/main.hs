import Data.List.Split
import Data.List

prepare :: String -> [Int]
prepare = map read . splitOn ","

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x:y:merge xs ys

average xs = fromIntegral (sum xs) `div` length xs

fuel n = sum . map (abs . (n-))

fuel2 n = sum . map (cost . abs . (n-))
    where cost n = sum $ take n [1..]

getTotalFuelBy f xs 
    = minimum 
    . take (length xs - av) 
    $ map (\x -> f (av+x) xs) (0 : merge [1,2..] [-1,-2..])
    where av = average xs

solve1 = getTotalFuelBy fuel
solve2 = getTotalFuelBy fuel2

main = do
    file <- readFile "input.txt"
    putStrLn $ "Part 1: "++(show . solve1 . prepare $ file)
    putStrLn $ "Part 2: "++(show . solve2 . prepare $ file)
