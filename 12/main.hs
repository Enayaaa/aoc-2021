import           Data.Char       (isUpper)
import           Data.List       (group, sort)
import           Data.List.Split (splitOn)
import           Data.Maybe      (isJust, isNothing)
import           Debug.Trace     (trace)


data Cave = Start | End | BigCave String | SmallCave String deriving (Eq, Ord)
data Edge = Edge {start :: Cave, end :: Cave} deriving (Eq)

instance Show Cave where
    show Start         = "Start"
    show End           = "End"
    show (BigCave s)   = s
    show (SmallCave s) = s

instance Show Edge where
    show (Edge s e) = "E "++show s++"->"++show e

toCave :: String -> Cave
toCave [] = error "Empty string parsed to Cave"
toCave s@(x:_)
    | s == "start" = Start
    | s == "end" = End
    | isUpper x = BigCave s
    | otherwise = SmallCave s

toEdge :: (String, String) -> Edge
toEdge (start, end) = Edge (toCave start) (toCave end)

prepare :: String -> [Edge]
prepare = map (toEdge . (\(x:y:_) -> (x,y)) . splitOn "-") . lines

canTraverse :: [Cave] -> Edge -> Bool
canTraverse visited edge = case end edge of
    (BigCave   _) -> True
    _             -> end edge `notElem` visited

neighbors :: Cave -> [Edge] -> [Edge]
neighbors node = filter (\e -> start e == node)

dfs :: [Edge] -> Int
dfs edges = dfs' edges [] Start

dfs' :: [Edge] -> [Cave] -> Cave -> Int
dfs' _     visited End  = 1
dfs' edges visited cave
    | isSmall cave || cave == Start = sum $ map (dfs' edges (cave:visited) . end) toTraverse
    | otherwise = sum $ map (dfs' edges visited . end) toTraverse
    where toTraverse = filter (\edge -> end edge `notElem` visited) $ neighbors cave edges

solve1 :: [Edge] -> Int
solve1 edges = dfs (edges++map (\(Edge s e) -> Edge e s) edges)

count :: Eq a => a -> [a] -> Int
count e = length . filter (==e)

canTraverse' :: [Cave] -> Edge -> Bool
canTraverse' visited edge = case end edge of
    Start           -> Start `notElem` visited
    End             -> End `notElem` visited
    (BigCave     _)   -> True
    c@(SmallCave _) -> if existsSmallCaveTwice visited
                        then c `notElem` visited
                        else count c visited < 2

isSmall :: Cave -> Bool
isSmall (SmallCave c) = True
isSmall _             = False

existsSmallCaveTwice :: [Cave] -> Bool
existsSmallCaveTwice = (>1) . length . smallCaveTwiceOccurences

smallCaveTwiceOccurences :: [Cave] -> [[Cave]]
smallCaveTwiceOccurences
    = filter ((>1) . length) . group . sort . filter isSmall

-- dfs2 :: [Edge] -> Int
-- dfs2 edges = dfs2' edges Nothing [] Start

-- dfs2' :: [Edge] -> Maybe Cave -> [Cave] -> Cave ->Int
-- dfs2' _     _     visited End  = trace (show $ reverse (End:visited)) 1

-- dfs2' edges twice visited cave
--     | isSmall cave = sum $ map (dfs2' edges (if isNothing twice && cave `elem` visited then Just cave else twice) (cave:visited) . end) toTraverse
--     | cave == Start = sum $ map (dfs2' edges twice visited . end) toTraverse
--     | otherwise = sum $ map (dfs2' edges twice (cave:visited) . end) toTraverse
--     where toTraverse = filter (\edge -> if isJust twice || end edge == Start then end edge `notElem` visited else  count (end edge) visited < 2) $ neighbors cave edges

dfs2 edges = dfs2' edges [] Start

dfs2' :: [Edge] -> [Cave] -> Cave ->Int
dfs2' _     visited End  = trace (show $ reverse (End:visited)) $ if length (smallCaveTwiceOccurences visited) > 1 then 0 else 1
dfs2' edges visited cave = sum $ map (dfs2' edges (cave:visited) . end) toTraverse
    where toTraverse = filter (canTraverse' visited) $ neighbors cave edges

solve2 :: [Edge] -> Int
solve2 edges = dfs2 (edges++map (\(Edge s e) -> Edge e s) edges)

main :: IO ()
main = do
    file <- readFile "input.txt"
    putStrLn $ "Part 1: "++(show . solve1 . prepare $ file)
    putStrLn $ "Part 2: "++(show . solve2 . prepare $ file)
