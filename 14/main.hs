import           Data.Function   (on)
import           Data.List       (groupBy, maximumBy, minimumBy, sort)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe      (fromMaybe)

type Template = String
type Pair = String
type Molecule = String
type InsertionRule = (Pair, Molecule)


step :: Map Pair Int -> Map Pair Int -> [InsertionRule] -> Map Pair Int
step old new [] = new
step old new (rule@(from,to):rules) = step old new' rules
    where
        n = old M.! from
        new' = if n == 0
            then new
            else M.insertWith (+) (to++[b]) n
             $ M.insertWith (+) (a:to) n
             $ M.insertWith (flip (-)) from n new
        (a:b:_) = from

count :: Eq a => a -> [a] -> Int
count e = length . filter (==e)

prepare :: String -> (Template, [InsertionRule])
prepare input = (template, rules)
    where (template:rulesStr:_) = splitOn "\n\n" input
          rules = map ((\(x:y:_) -> (x,y)) . splitOn " -> ") $ lines rulesStr

clean :: M.Map a Int -> [(a, Int)]
clean = filter ((>0) . snd) . M.toList

solve :: Int -> Template -> [InsertionRule] -> Int
solve n template rules = most - least
    where
        -- Brace yourself
        pairs' = M.fromList $ map (\l -> (l,count l pairs)) letters
        pairs = zipWith (\a b -> [a, b]) template (tail template)
        letters = [[a, b] | a <- ['A'..'Z'], b <- ['A'..'Z']]
        res = clean . (!!n) $ iterate (\ps -> step ps ps rules) pairs'
        xs' = M.insertWith (+) (last template) 1 xs
        xs = M.fromList $ map (\gr -> (head (fst $ head gr),foldl (\c (_,n) -> n+c) 0 gr))
            . groupBy (\(x:_,_) (y:_,_) -> x == y) $ sort res
        (mostCommon, most) = maximumBy (compare `on` snd) $ M.toList xs'
        (leastCommon, least) = minimumBy (compare `on` snd) $ M.toList xs'

main :: IO ()
main = do
    file <- readFile "test_input.txt"
    putStrLn $ "Part 1: "++(show . uncurry (solve 10) . prepare $ file)
    putStrLn $ "Part 2: "++(show . uncurry (solve 40) . prepare $ file)
