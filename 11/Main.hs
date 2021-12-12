module Main where

-- Using priority queue for some reason, is not needed, better to use Map,
-- I thaught it would be needed but the priority queue is not even used as it
-- is supposed to.

import           Data.Char            (digitToInt)
import           Data.List            (sortBy)
import qualified Data.PQueue.Prio.Max as P
import qualified Data.Set             as Set

prepare :: String -> [[Int]]
prepare = map (map digitToInt) . lines

isValidPoint :: [[Int]] -> (Int, Int) -> Bool
isValidPoint xs (x,y)
    =  x >= 0 && x < length xs
    && y >= 0 && y < length (head xs)

neighbours :: [[Int]] -> (Int, Int) -> [(Int, Int)]
neighbours xs (x,y)
    = filter (isValidPoint xs)
    [(x+1,y),(x+1,y+1),(x+1,y-1),(x,y+1),(x,y-1),(x-1,y+1),(x-1,y),(x-1,y-1)]

getPoint :: [[a]] -> (Int, Int) -> a
getPoint xs (x,y) = xs !! x !! y

willFlash :: [[Int]] -> (Int, Int) -> Bool
willFlash xs p = getPoint xs p == 9

points :: [[a]] -> [(Int, Int)]
points xs = [(x,y) | x <- [0..width], y <- [0..height]]
    where width = length (head xs) - 1
          height = length xs - 1

neighs :: [[Int]] -> (Int, (Int, Int)) -> [(Int, (Int, Int))]
neighs xs (k,v) = zip ps ns
    where ns = neighbours xs v
          ps = map (getPoint xs) ns

count :: Eq a => a -> [a] -> Int
count e = length . filter (==e)

flash :: [[Int]] -> P.MaxPQueue Int (Int,Int) -> Set.Set (Int, Int) -> (P.MaxPQueue Int (Int,Int), Set.Set (Int,Int))
flash xs pq flashed
    | null flashing = (incs, allFlashed)
    | otherwise     = flash (pqToMatrix incs) incs allFlashed
    where
        allFlashed = Set.union flashing flashed
        incs = P.foldlWithKey (\q k v -> if v `elem` toInc then P.insert (k+count v toInc) v q else P.insert k v q) P.empty pq
        toInc = filter (`Set.notMember` flashed)
              $ concatMap (neighbours xs) flashing
        flashing = Set.fromList
                 . map snd
                 . filter (\(k,v) -> v `Set.notMember` flashed)
                 $ toFlash pq

step :: P.MaxPQueue Int (Int, Int) -> (Int, P.MaxPQueue Int (Int, Int))
step pq = (length flashed, P.foldlWithKey (\q k v -> if Set.member v flashed then P.insert 0 v q else P.insert k v q) P.empty result)
    where
        incs = P.mapKeys (+1) pq
        (result, flashed) = flash (pqToMatrix pq) incs Set.empty

toFlash :: P.MaxPQueue Int (Int,Int) -> [(Int, (Int,Int))]
toFlash = P.takeWhileWithKey (\k x -> k>9)

counting :: Int -> Int -> P.MaxPQueue Int (Int, Int) -> Int
counting 0 s _ = s
counting n s p = counting (n-1) (s+c) newp
    where (c,newp) = step p

solve1 :: [[Int]] -> Int
solve1 xs
    = counting 100 0
    . P.fromList
    $ zip (concat xs) (points xs)

pqToMatrix :: P.MaxPQueue Int (Int,Int) -> [[Int]]
pqToMatrix
    = takeBy 10
    . map snd
    . sortBy (\(k,v) (k2,v2) -> k `compare` k2)
    . map (\(k,v) -> (v,k))
    . P.toList

takeBy :: Int -> [a] -> [[a]]
takeBy _ [] = []
takeBy n ls = take n ls : takeBy n (drop n ls)

foo :: Int -> P.MaxPQueue Int (Int, Int) -> Int
foo n p
    | P.size (P.dropWhileWithKey (\k v -> k==0) newp) == 0 = n
    | otherwise = foo (n+1) newp
    where (c,newp) = step p

solve2 :: [[Int]] -> Int
solve2 xs = foo 1 $ P.fromList $ zip (concat xs) (points xs)

main :: IO ()
main = do
    file <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show . solve1 . prepare $ file)
    putStrLn $ "Part 2: " ++ (show . solve2 . prepare $ file)

