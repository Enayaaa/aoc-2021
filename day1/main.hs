
import Data.Char

countInc :: (Ord a, Num a) => [a] -> Int
countInc inp = length $ filter (>0) $ zipWith (\a b -> b-a) inp (tail inp)

countTrippleInc :: (Ord a, Num a) => [a] -> Int
countTrippleInc inp = countInc sums
            where
                sums = zipWith3 (\a b c -> a+b+c) inp (drop 1 inp) (drop 2 inp)

main = do
    inp <- readFile "input.txt"
    let arr = map (\x -> read x :: Integer) (lines inp) 
    putStrLn $ "part 1: " ++ show (countInc arr)
    putStrLn $ "part 2: " ++ show (countTrippleInc arr)
