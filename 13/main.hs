import           Data.List       (maximumBy)
import           Data.List.Split (splitOn)

type Point = (Int,Int)

prepare :: String -> ([(String,Int)], [Point])
prepare str = (fls,pts)
    where
        lns = map lines .  splitOn "\n\n" $ str
        pts = map ((\(a:b:_) -> (a,b)) . map read . splitOn ",") $ head lns
        fls = map ((\(a:b:_) -> (a, read b)) . splitOn "=" . drop 11) $ last lns

fold :: [Point] -> ([Char], Int) -> [Point]
fold points (along, line) = case along of
    "x" -> if xDiff <= 0 then folded else map (\(x,y) -> (x+abs xDiff,y)) folded
    "y" -> if yDiff <= 0 then folded else map (\(x,y) -> (x,y+abs yDiff)) folded
    _   -> error "unknownn stuff"
    where
        folded = foldl (\xs p -> project along line p : xs) [] points
        xDiff = maxX points - line - line
        yDiff = maxY points - line - line

project :: String -> Int -> Point -> Point
project perp line (x,y) = case perp of
    "x" -> if x < line then (x,y) else (line-(x-line),y)
    "y" -> if y < line then (x,y) else (x,line-(y-line))
    _   -> error "unknown stuff"

maxX :: [Point] -> Int
maxX = fst . maximumBy (\(x,y) (a,b) -> compare x a)

maxY :: [Point] -> Int
maxY = snd . maximumBy (\(x,y) (a,b) -> compare y b)

takeBy :: Int -> [a] -> [[a]]
takeBy _ [] = []
takeBy n ls = take n ls : takeBy n (drop n ls)

paint :: [Point] -> [String]
paint points
    = takeBy (maxX points+1)
    $ map (\p -> if p `elem` points then '#' else '.') idx
    where
          idx = [(x,y) | y <- [0..maxY points], x <- [0..maxX points]]

count :: Eq a => a -> [a] -> Int
count e = length . filter (==e)

solve1 :: ([(String, Int)], [Point]) -> Int
solve1 (folds, points)
    = sum
    . map (count '#')
    . paint
    $ fold points (head folds)

solve2 :: ([(String, Int)], [Point]) -> [String]
solve2 (folds, points) = paint $ foldl fold points folds

main :: IO ()
main = do
    file <- readFile "input.txt"
    putStrLn $ "Part 1: "++(show . solve1 . prepare $ file)
    putStrLn $ "Part 2: "++(show . solve2 . prepare $ file)

{-
[
"..##.###..####.###..#.....##..#..#.#..#",
"...#.#..#....#.#..#.#....#..#.#.#..#..#",
"...#.#..#...#..###..#....#....##...####",
"...#.###...#...#..#.#....#.##.#.#..#..#",
"#..#.#.#..#....#..#.#....#..#.#.#..#..#",
".##..#..#.####.###..####..###.#..#.#..#"]
-}
