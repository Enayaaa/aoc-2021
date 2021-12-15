module Main where
import           Data.Char
import           Data.Map             (Map)
import qualified Data.Map             as M
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set             as S
import           Debug.Trace

prepare input = xs
    where
        xs = map (map digitToInt) $ lines input

isValidPoint row col (x,y) = x >= 0 && x < col && y >= 0 && y < row

neighbours row col (x,y)
    = filter (isValidPoint row col) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

type Point = (Int,Int)
type Cost = Int

dijkstras :: Map Point Cost -> Int -> Int -> S.Set Point -> P.MinPQueue Cost Point -> Int
dijkstras m row col visited pq
    | P.null pq = error "no path found"
    | v == destination = cost
    | v `S.notMember` visited
        = trace (show destination) dijkstras m row col visited' (foldl (\q n -> P.insert (cost  + (m M.! n)) n q) pq' toVisit)
    | otherwise = dijkstras m row col visited' pq'
    where
        toVisit = filter (`S.notMember` visited) $ neighbours row col v
        visited' = S.insert v visited
        destination = (col-1,row-1)
        ((cost,v), pq') = P.deleteFindMin pq


solve1 xs = dijkstras m row col S.empty pq
    where
        m = M.fromList $ zip ps (concat xs)
        pq = P.singleton 0 (0,0)
        ps = [(x,y) | x <- [0..row-1], y <-[0..col-1]]
        (row, col) = (length xs, length (head xs))

addAll x = map (\n -> if (n+x) > 9 then 1 else n+x)

addAll' x = concatMap (addAll x)

solve2 xs = ys--dijkstras m row col S.empty pq
    where
        m = M.fromList $ zip ps (concat xs')
        xs' = foldl ys [1..4] --ys++[addAll 1 z | z <- ys]++[addAll 2 z | z <- ys]++[addAll 3 z | z <- ys]++[addAll 4 z | z <- ys]
        ys = map (\r -> r++addAll 1 r++addAll 2 r++addAll 3 r++addAll 4 r) xs 
        pq = P.singleton 0 (0,0)
        ps = [(x,y) | x <- [0..col-1], y <-[0..row-1]]
        (row, col) = (length xs', length (head xs'))

main :: IO ()
main = do
    file <- readFile "test_input.txt"
    --putStrLn $ "Part 1: " ++ (show . solve1 . prepare $ file)
    putStrLn $ "Part 2: " ++ (show . solve2 . prepare $ file)

{-

[
[1,1,6,3,7,5,1,7,4,2,2,2,7,4,8,6,2,8,5,3,3,3,8,5,9,7,3,9,6,4,4,4,9,6,2,8,4,2,7,5,5,5,3,7,3,9,5,3,8,6],
[1,3,8,1,3,7,3,6,7,2,2,4,9,2,4,8,4,7,8,3,3,5,1,3,5,9,5,8,9,4,4,6,2,4,6,2,6,9,2,5,5,7,3,5,7,3,7,3,3,6],
[2,1,3,6,5,1,1,3,2,8,3,2,4,7,6,2,2,4,3,9,4,3,5,8,7,3,3,5,4,1,5,4,6,9,8,4,4,6,5,2,6,5,7,3,9,5,5,7,6,3],
[3,6,9,4,9,3,1,5,6,9,4,7,0,5,0,4,2,6,7,0,5,8,1,6,1,5,3,7,8,1,6,9,2,7,2,6,4,8,9,2,7,3,3,8,3,7,5,9,3,3],
[7,4,6,3,4,1,7,1,1,1,8,5,7,4,5,2,8,2,2,2,9,6,8,5,6,3,9,3,3,3,2,7,9,6,7,4,2,4,4,4,3,8,3,7,8,5,3,5,5,5],
[1,3,1,9,1,2,8,1,3,7,2,4,2,0,2,3,9,2,4,8,3,5,3,1,3,4,1,3,5,9,4,6,4,2,4,5,2,4,6,2,5,7,5,3,5,6,3,5,7,3],
[1,3,5,9,9,1,2,4,2,1,2,4,6,0,0,2,3,5,3,2,3,5,7,1,1,3,4,6,4,3,4,6,8,2,2,4,5,7,5,4,5,7,9,3,3,5,6,8,6,5],
[3,1,2,5,4,2,1,6,3,9,4,2,3,6,5,3,2,7,4,0,5,3,4,7,6,4,3,8,5,1,6,4,5,8,7,5,4,9,6,2,7,5,6,9,8,6,5,3,7,3],
[1,2,9,3,1,3,8,5,2,1,2,3,0,4,2,4,9,6,3,2,3,4,1,5,3,5,1,7,4,3,4,5,2,6,4,6,2,8,5,4,5,6,3,7,5,7,3,9,6,5],
[2,3,1,1,9,4,4,5,8,1,3,4,2,2,0,5,5,6,9,2,4,5,3,3,1,6,6,7,1,3,5,6,4,4,2,7,7,8,2,4,6,7,5,5,3,8,8,9,3,5],
[2,2,7,4,8,6,2,8,5,3,3,3,8,5,9,7,3,9,6,4,4,4,9,6,0,8,4,0,7,5,5,5,0,7,3,9,5,3,8,6,6,6,4,8,4,0,6,4,9,7],
[2,4,9,2,4,8,4,7,8,3,3,5,0,3,5,9,5,8,9,4,4,6,2,4,6,0,6,9,0,5,5,7,3,5,7,3,7,0,3,6,6,8,4,6,8,4,8,4,4,7],
[3,2,4,7,6,2,2,4,3,9,4,3,5,8,7,3,3,5,4,0,5,4,6,9,8,4,4,6,5,2,6,5,7,0,9,5,5,7,6,3,7,6,8,4,0,6,6,8,7,4],
[4,7,0,5,0,4,2,6,7,0,5,8,1,6,1,5,3,7,8,1,6,9,2,7,2,6,4,8,9,2,7,0,3,8,3,7,5,9,0,3,8,4,4,9,4,8,6,0,4,4],
[8,5,7,4,5,2,8,2,2,2,9,6,8,5,6,3,9,3,3,3,0,7,9,6,7,4,0,4,4,4,3,8,0,7,8,5,3,5,5,5,4,9,4,8,9,6,4,6,6,6],
[2,4,2,0,2,3,9,2,4,8,3,5,3,1,3,4,0,3,5,9,4,6,4,2,4,5,2,4,6,0,5,7,5,3,5,6,3,5,7,3,6,8,6,4,6,7,4,6,8,4],
[2,4,6,0,0,2,3,5,3,2,3,5,7,1,1,3,4,6,4,3,4,6,8,2,2,4,5,7,5,4,5,7,9,3,3,5,6,8,6,5,6,8,0,4,4,6,7,9,7,6],
[4,2,3,6,5,3,2,7,4,0,5,3,4,7,6,4,3,8,5,1,6,4,5,8,7,5,4,9,6,2,7,5,6,9,8,6,5,0,7,3,8,6,7,0,9,7,6,4,8,4],
[2,3,0,4,2,4,9,6,3,2,3,4,1,5,3,5,0,7,4,3,4,5,2,6,4,6,2,8,5,4,5,6,3,7,5,7,3,9,6,5,6,7,4,8,6,8,4,0,7,6],
[3,4,2,2,0,5,5,6,9,2,4,5,3,3,1,6,6,7,0,3,5,6,4,4,2,7,7,8,2,4,6,7,5,5,3,8,8,9,3,5,7,8,6,6,4,9,9,0,4,6]
]

-}
