
import Data.Char

countInc :: (Ord a, Num a) => [a] -> Int
countInc inp = length $ filter (> 0) $ map (\(a,b) -> b-a) $ zip inp (drop 1 inp)

countTrippleInc :: (Ord a, Num a) => [a] -> Int
countTrippleInc inp = length $ filter (>0) $ map (\(a,b) -> b-a) $ zip sums (drop 1 sums)
            where
                sums = map (\(a,b,c) -> a+b+c) $ zip3 inp (drop 1 inp) (drop 2 inp)

main = do
    inp <- readFile "input.txt"
    let arr = map (\x -> read x :: Integer) (lines inp) 
    putStr "part 1: "
    putStrLn $ show (countInc arr)
    putStr "part 2: "
    putStrLn $ show (countTrippleInc arr)

