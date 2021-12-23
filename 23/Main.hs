module Main where

-- This code sadly doen't work as of now (its not fast enough for the real input)
-- So I gave up and did both parts manually

import Data.List.Split
import Data.Char
import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Function
import qualified Data.PQueue.Prio.Min as P

data Amphipod = A | B | C | D deriving (Eq, Show, Ord)
data Space = Occupied Amphipod | Open | Wall deriving (Eq, Show)
type Location = (Int,Int)
type Diagram = Map Location Space

padLine :: Int -> String -> String
padLine cs l
    | length l == cs = l
    | otherwise = let n = (cs - length l) `div` 2 in
        replicate n '#'++l++replicate n '#'

parseSpace '#' = Wall
parseSpace '.' = Open
parseSpace 'A' = Occupied A
parseSpace 'B' = Occupied B
parseSpace 'C' = Occupied C
parseSpace 'D' = Occupied D
parseSpace x = error $ "unknown space `"++[x]++"`"

prepare :: String -> Diagram
prepare input = M.fromList . concat $ zipWith zip ps padded
    where
    ls = lines input
    rs = length ls
    cs = length (head ls)
    ps = [[(r,c) | c <-[0..cs-1]] | r <- [0..rs-1]]
    padded = map (map parseSpace . padLine cs . dropWhile isSpace) ls

outsideADoor l = l `elem` [(1,3),(1,5),(1,7),(1,9)]

emptySpots = M.filterWithKey (\k v -> not $ outsideADoor k) . M.filter (== Open)

roomFor A = [(2,3),(3,3)]
roomFor B = [(2,5),(3,5)]
roomFor C = [(2,7),(3,7)]
roomFor D = [(2,9),(3,9)]

stepCost A = 1
stepCost B = 10
stepCost C = 100
stepCost D = 1000

isInHallway (r,c) = r == 1

occupied (Occupied _) = True
occupied _ = False

amphipods = M.filter occupied

pathLength a b = length $ path a b

isPathBlocked p m = any (\x -> (m M.! x) /= Open) p

move m a@(l1, s1@(Occupied x)) b@(l2, s2)
    | isPathBlocked p m = Nothing
    | isInHallway l1 && isInHallway l2 = Nothing
    | snd l1 == snd l2 = Nothing
    | isInHallway l1 && not (isDone m (l2,s1)) = Nothing
    | not (isInHallway l2) && not (isDone m (l2,s1)) = Nothing
    | fst l2 == 2 && m M.! (3, snd l2) == Open = Nothing
    | otherwise = Just (stepCost x*pathLength a b, m')
    where
    room = map (m M.!) ps
    ps = roomFor x

    p = path a b
    Occupied x = s1
    m' = if isDone m (l2, s1)
        then M.insert l1 Open $ M.insert l2 Wall m
        else M.insert l1 Open $ M.insert l2 s1 m
move _ _ _ = Nothing

path ((r1,c1), s1) ((r2,c2), s2)
    = filter (/=(r1,c1))
    $ zip [2..r1] (repeat c1)
    ++ zip (repeat 1) [min c1 c2..max c1 c2]
    ++ zip [2..r2] (repeat c2)

isDone :: Diagram -> (Location, Space) -> Bool
isDone m ((r,c),Occupied a)
    | r == 2 = (r,c) `elem` ps && (under == Wall || under == Occupied a)
    | r == 3 = (r,c) `elem` ps
    | otherwise = False
    where
    under = m M.! (3,c)
    room = map (m M.!) ps
    ps = roomFor a
isDone _ _ = error "calling isDone with non-amphipods"

simulate :: P.MinPQueue Int Diagram -> Maybe (Int, Diagram)
simulate pq
    | all (isDone m) as = Just (c,m)
    --  null recur = Nothing
    | otherwise = let res = simulate (foldl (\q (c',m') -> P.insert c' m' q) pq' xs) in
        case res of
            Nothing -> Nothing
            a -> a
    where
    --recur = mapMaybe (\(c',m') -> simulate (c+c', m')) xs
    xs = map (\(c',m') -> (c+c',m')) $ mapMaybe (uncurry (move m)) ys
    ys = [(a,b) | a <- as, b <- empty]
    empty = M.toList $ emptySpots m
    as = M.toList $ amphipods m
    ((c,m), pq') = P.deleteFindMin pq

isAmphipod (Occupied _) = True
isAmphipod _ = False

toAmphipod (Occupied x) = x
toAmphipod _ = error "not an amphipod"

solve1 m = --trace (showDiagram m') 0--isDone m' ((2,9), Occupied D)
    --move m' ((1,10), Occupied D) ((2,9), Occupied D)
    {-(\(Just x) -> trace ('\n':showDiagram (snd x)) x ) $-} fst . fromJust $ simulate (P.singleton 0 m')
    --map (map (map simulate . simulate) . simulate) (simulate (0,m))
    --uncurry traceShow $ move m (head start) (head empty)
    where
    m' = M.foldlWithKey (\m' k v -> if isAmphipod v then let Occupied x = v in if isDone m (k,v) then M.insert k Wall m' else M.insert k v m' else M.insert k v m') M.empty m
    a = head start
    b = head empty
    empty = M.toList $ emptySpots m
    start = M.toList $ M.filterWithKey (\(r,c) v -> r == 2 && occupied v) m

solve2 = id

main = do
    --file <- readFile "test_input.txt"
    --file <- readFile "sample.txt"
    file <- readFile "input.txt"
    putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
    --putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)
    --putStrLn . showDiagram . solve1 . prepare $ file


spaceToChar Wall        = '#'
spaceToChar Open        = '.'
spaceToChar (Occupied A) = 'A'
spaceToChar (Occupied B) = 'B'
spaceToChar (Occupied C) = 'C'
spaceToChar (Occupied D) = 'D'

showDiagram m = intercalate "\n" . map (map (spaceToChar . snd)) . groupBy (\a b -> fst (fst a) == fst (fst b)) $ M.toList m






