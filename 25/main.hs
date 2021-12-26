import           Data.List   (groupBy, intercalate)
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Debug.Trace (trace)

data Occupent = EastC | SouthC | None deriving (Show, Eq)

readOccupent :: Char -> Occupent
readOccupent '>' = EastC
readOccupent 'v' = SouthC
readOccupent '.' = None
readOccupent _   = error "Unknown Occupent, maybe it's a deep sea alien?"

prepare :: [Char] -> (Map (Int, Int) Occupent, (Int, Int))
prepare input = (M.fromList $ zip points (concat parsed), (length parsed, length (head parsed)))
    where rs = lines input
          parsed = map (map readOccupent) rs
          points = [(r,c) | r <- [0..length parsed-1], c <- [0..length (head parsed)-1]]

destination :: (Int,Int) -> Occupent -> (Int,Int) -> (Int,Int)
destination (r,c) EastC (rs,cs)  = let c' = (c+1) `mod` cs in (r,c')
destination (r,c) SouthC (rs,cs) = let r' = (r+1) `mod` rs in (r',c)
destination (r,c) _ _            = (r,c)

moveEastCs m (rs,cs) l k@(r,c) EastC
    | m M.! d == None = (k,None):(d,EastC):l
    | otherwise = l
    where d@(r',c') = destination k EastC (rs,cs)
moveEastCs m _ l k v = l

moveSouthCs m (rs,cs) l k@(r,c) SouthC
    | m M.! d == None = (k,None):(d,SouthC):l
    | otherwise = l
    where d@(r',c') = destination k SouthC (rs,cs)
moveSouthCs m _ l k v = l

step :: Map (Int, Int) Occupent -> (Int, Int) -> Map (Int, Int) Occupent
step m (rs, cs) = M.union (M.fromList l') m'
    where m' = M.union (M.fromList l) m
          l = M.foldlWithKey (moveEastCs m (rs,cs)) [] m
          l' = M.foldlWithKey (moveSouthCs m' (rs,cs)) [] m'

loop :: Map (Int, Int) Occupent -> (Int, Int) -> Int
loop m (rs, cs) = loop' 1 m
    where loop' n m
            | m' == m = n
            | otherwise = trace (showMap m++"\n") loop' (n+1) m'
            where m' = step m (rs,cs)

solve1 :: Map (Int, Int) Occupent -> (Int, Int) -> Int
solve1 = loop

showOccupent :: Occupent -> Char
showOccupent EastC  = '>'
showOccupent SouthC = 'v'
showOccupent None   = '.'

showMap :: Map (Int,Int) Occupent -> String
showMap m = intercalate "\n"
    . map (map (\(_,v) -> showOccupent v))
    . groupBy (\a b -> (fst . fst) a == (fst . fst) b)
    $ M.toList m

main :: IO ()
main = do
    --file <- readFile "test_input.txt"
    file <- readFile "input.txt"
    putStrLn $ "Part 1 :" ++ (show . uncurry solve1 . prepare $ file)
