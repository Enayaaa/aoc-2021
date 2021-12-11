module Main where

import Debug.Trace
import Data.Char
import qualified Data.PQueue.Prio.Max as P
import qualified Data.Set as Set

prepare :: String -> [[Int]]
prepare = map (map digitToInt) . lines

isValidPoint xs (x,y) = x >= 0 && x < length xs && y >= 0 && y < length (head xs)

neighbours xs (x,y)
    = filter (isValidPoint xs)
    [(x+1,y),(x+1,y+1),(x+1,y-1),(x,y+1),(x,y-1),(x-1,y+1),(x+1,y),(x+1,y-1)]

getPoint xs (x,y) = xs !! x !! y

willFlash xs p = getPoint xs p == 9

points xs = [(x,y) | x <- [0..width], y <- [0..height]]
    where width = length (head xs) - 1
          height = length xs - 1

-- step xs = map (map (\x -> if x > 9 then 0 else x)) incs
    -- where incs = map (map (+1)) xs
          -- toFlash = map (filter (>9)) incs
neighs xs (k,v) = zip ps ns
    where ns = neighbours xs v
          ps = map (getPoint xs) ns

flash xs pq flashed = toInc
    where
        toInc = filter (\(k,v) -> Set.notMember v flashed) $ concatMap (neighs xs) (toFlash pq)
        incs = map (\(k,v) -> (k+1,v)) toInc
        flashing = toFlash 

step xs pq = updated
    where
        updated = P.mapKeys (+1) pq
        flashing = Set.fromList $ map snd $ toFlash updated
        foo = flash xs updated flashing

        flashed = P.foldlWithKey (\q k p -> P.insert (k+1) p q) P.empty pq

toFlash = P.takeWhileWithKey (\k x -> k>9)
 
solve1 xs = toFlash . step xs . step xs $ P.fromList $ zip (concat xs) (points xs)


main = do 
    file <- readFile "short_input.txt"
    putStrLn $ "Part 1: " ++ (show . solve1 . prepare $ file)
