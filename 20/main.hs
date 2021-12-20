import           Data.Function      (on)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap -- because O(min(n,W)) acess
import           Data.List          (sortBy)
import           Data.List.Split    (splitOn)

type Image = [String]
type Point = (Int, Int)
type Rules = IntMap Char
type Bit = Int

lightPixels :: Image -> [Point]
lightPixels xs = ps
    where
        ps = [(r,c) | r <- [0..rows], c <- [0..cols], xs !! r !! c == '#']
        rows = length xs - 1
        cols = length (head xs) - 1

prepare :: String -> (Rules, Image)
prepare input = (IntMap.fromList $ zip [0..511] algo, image)
    where
        (algo:xs) = splitOn "\n\n" input
        image = concatMap lines xs

surrounding :: Point -> [Point]
surrounding (r,c) =
    [(r-1,c-1),(r-1,c),(r-1,c+1),(r,c-1),(r,c),(r,c+1),(r+1,c-1),(r+1,c),(r+1,c+1)]

toDec :: [Bit] -> Int
toDec = foldl ((+) . (*2)) 0

isOutside :: Point -> Point -> Point -> Bool
isOutside (minR,maxR) (minC,maxC) (r,c) =
    r < minR || r > maxR || c < minC || c > maxC

enhancePixels :: Rules -> Bit -> Point -> Point -> [Point] -> [Point] -> [Point] -> [Point]
enhancePixels _ _ _ _ _ acc [] = acc
enhancePixels m o r c lights acc (p:ps) = case m IntMap.! idx of
    '#' -> p:enhancePixels m o r c lights acc ps
    '.' -> enhancePixels m o r c lights acc ps
    _   -> error "unknown pixel found"
    where
        idx = toDec $ map (\p -> if isOutside r c p then o else if p `elem` lights then 1 else 0)
            $ surrounding p

minmaxRowIdx :: [Point] -> (Int, Int)
minmaxRowIdx ps = let sorted = sortBy (compare `on` fst) ps in
    (fst (head sorted), fst (last sorted))

minmaxColIdx :: [Point] -> (Int,Int)
minmaxColIdx ps = let sorted = sortBy (compare `on` snd) ps in
    (snd (head sorted), snd (last sorted))

enhance :: Rules -> (Bit, [Point]) -> (Bit, [Point])
enhance m (o,lights) = (toggle o, enhancePixels m o r c lights [] ps)
    where
        ps = [(r,c) | r <- [minR-1..maxR+1], c <- [minC-1..maxC+1]]
        r@(minR,maxR) = minmaxRowIdx lights
        c@(minC,maxC) = minmaxColIdx lights

toggle :: Bit -> Bit
toggle i
    | i == 0 = 1
    | otherwise = 0

solve1 :: (Rules, Image) -> Int
solve1 (m, i) = length . snd . enhance m $ enhance m (0,lights)
    where
        lights = lightPixels i

solve2 :: (Rules, Image) -> Int
solve2 (m, i) = length . snd . (!!50) $ iterate (enhance m) (0,lights)
    where
        lights = lightPixels i

main :: IO ()
main = do
    --file <- readFile "test_input.txt"
    file <- readFile "input.txt"
    putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
    putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)
-- This solution is pretty bad, it is too inefficient and takes 10min for part2
-- Don't know how I could make it faster.
