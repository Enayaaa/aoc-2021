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
        = dijkstras m row col visited' (foldl (\q n -> P.insert (cost + (m M.! n)) n q) pq' toVisit)
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

getNext = addAll 1

solve2 xs = dijkstras m row col S.empty pq
    where
        m = M.fromList $ zip ps (concat xs')
        xs' = concat . take 5 $ iterate (map getNext) ys
        ys = map (\x -> foldl (\r _ -> x++getNext r) (take col' x) [1..4])  xs
        pq = P.singleton 0 (0,0)
        col' = length xs
        ps = [(x,y) | x <- [0..col-1], y <-[0..row-1]]
        (row, col) = (length xs', length (head xs'))

main :: IO ()
main = do
    file <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show . solve1 . prepare $ file)
    putStrLn $ "Part 2: " ++ (show . solve2 . prepare $ file)

