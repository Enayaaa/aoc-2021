import Data.List.Split
import qualified Data.Map as M
import Data.List hiding (insert)
import Data.Function (on)
import qualified Data.Set as S
import Data.Maybe

prepare input = (template, M.fromList rules)
    where (template:rulesStr:_) = splitOn "\n\n" input
          rules = map ((\(x:y:_) -> (x,y)) . splitOn " -> ") $ lines rulesStr

insert :: M.Map String String -> String -> (Char,Char) -> String
insert rules acc (a,b) = case M.lookup [a,b] rules of
    Just x -> acc++x++[b]
    Nothing -> acc++[b]

step :: M.Map String String -> String -> String
step rules template = foldl (insert rules) (take 1 template) xs
    where
        xs = zip template (tail template)

solve1 (template, rules) = res--length (last xs) - length (head xs)
    where
        res = (!! 2) $ iterate (step rules) template
        xs = sortBy (compare `on` length) . group $ sort res

insert' :: M.Map String String -> M.Map String Int -> M.Map String Int -> [(String, Int)] -> M.Map String Int
insert' _ _ acc [] = acc
insert' rules m acc (pair@(p@(a:b:_),count) : pairs) = case M.lookup p rules of
    Just x -> insert' rules m (
        M.insert (a:x) (ax+1) $
        M.insert (x++[b]) (xb+1) $
        M.insert p (ab-1) acc
        ) pairs
        where
            ax = let ax' = M.lookup (a:x) acc in fromMaybe 0 ax'
            xb = let xb' = M.lookup (x++[b]) acc in fromMaybe 0 xb'
            ab = let ab' = M.lookup p acc in fromMaybe 0 ab'
    Nothing -> insert' rules m acc pairs
insert' _ _ _ _ = error "incorrect input"

--step' :: M.Map String String -> M.Map String Int -> M.Map String Int
step' rules pairs = insert' rules pairs pairs (M.toList pairs)

count e = length . filter (==e)

solve2 (template, rules) = pairs
    where
        temp = map (\x -> (count x template, x)) $ nub template
        -- pairs = zipWith (\a b -> [a, b]) template (tail template)
        -- pairs' = foldl (\m x -> M.insert x (count x pairs) m) M.empty pairs
        pairs = M.fromList [([a, b],0) | a <- ['A'..'Z'], b <- ['A'..'Z']]


main = do
    file <- readFile "test_input.txt"
    --putStrLn $ "Part 1: "++(show . solve1 . prepare $ file)
    putStrLn $ "Part 2: "++(show . solve2 . prepare $ file)
