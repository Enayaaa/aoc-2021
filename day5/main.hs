{-
 - This is a pretty shitty solution according to me
 -
 - I first try to go the path of comparing each combination of pairs of lines
 - and see how many points they both intersect, this showed to be EXTREMELY
 - slow, I waited for 5 minutes before I tried to guess the complexity which
 - was something like O((n choose 2)*n^2) or something, idk, but it would've
 - surely taken a few hundred/thousand hours or so to run.
 -
 - This one took a few minutes.
 -}


import           Data.Char       (isSpace)
import           Data.List.Split (dropBlanks, dropDelims, oneOf, split, splitOn)
import           Data.Maybe      (fromJust, isJust)


-- | splits, removes the delimeter and any other delimeter element
--   that still is left in the list
splitOn' :: String -> String -> [String]
splitOn' delim = split (dropDelims . dropBlanks $ oneOf delim)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

count :: (Eq a) => a -> [a] -> Int
count e = length . filter (==e)

pairs :: [a] -> [(a, a)]
pairs = map (\(x:y:ys) -> (x,y)) . combinations 2

-- Stolen from (https://rosettacode.org/wiki/Combinations#Haskell)
combinations :: Int -> [a] -> [[a]]
combinations m xs = combBySize xs !! m
 where
   combBySize = foldr f ([[]] : repeat [])
   f x next = zipWith (++) (map (map (x:)) ([]:next)) next

-- Stolen from stackoverflow (https://stackoverflow.com/a/53838631)
-- Either replace the old first element with the new first element,
-- or insert the new element into the tail of the list and prepend
-- the old first element.
insert1D :: a -> Int -> [a] -> [a]
insert1D x' 0 (_:xs) = x':xs
insert1D x' p (x:xs) = x : insert1D x' (p - 1) xs

-- Stolen from stackoverflow (https://stackoverflow.com/a/53838631)
-- Likewise, either replace the old first row with a new first row
-- (which you get using insert1D), or insert the new element into
-- the tail of the array and prepend the old first row.
insert2D :: a -> Int -> Int -> [[a]] -> [[a]]
insert2D x'  0 py (r:rs) = insert1D x' py r : rs
insert2D x' px py (r:rs) = r : insert2D x' (px - 1) py rs


data Point = Point Int Int deriving (Show, Eq)
data Line = Line Point Point deriving (Show, Eq)
type Canvas = [[Int]]

isStraitLine :: Line -> Bool
isStraitLine (Line (Point x1 y1) (Point x2 y2)) = x1 == x2 || y1 == y2

isDiagonalLine :: Line -> Bool
isDiagonalLine (Line (Point x1 y1) (Point x2 y2)) = abs (x2-x1) == abs (y2 - y1)

getLineGradient :: Line -> Maybe Int
getLineGradient (Line (Point x1 y1) (Point x2 y2))
    | x2 == x1 = Nothing
    | otherwise = Just $ (y2 - y1) `div` (x2 - x1)

getLineYIntercept :: Line -> Maybe Int
getLineYIntercept (Line (Point x1 y1) p2)
    = case getLineGradient (Line (Point x1 y1) p2) of
        Just k  -> Just $ y1 - k*x1
        Nothing -> Nothing

getLineEq :: Line -> Maybe (Int, Int)
getLineEq l = case (k,m) of
                (Just k, Just m) -> Just (k,m)
                _                -> Nothing
    where k = getLineGradient l
          m = getLineYIntercept l

lineTrace :: Line -> [Point]
lineTrace l
    = zipWith Point (takeWhile isWithinXBoundry xs) (takeWhile isWithinYBoundry ys)
    where
        km = getLineEq (Line (Point x1 y1) (Point x2 y2))
        (Line (Point x1 y1) (Point x2 y2)) = l
        isWithinXBoundry x = x <= max x1 x2 && x >= min x1 x2
        isWithinYBoundry y = y <= max y1 y2 && y >= min y1 y2

        (xs,ys) = case km of
            Just (k,m) -> ([min x1 x2 + dx | dx <- [0..]], [k*x+m | x <- xs])
            Nothing    -> (repeat x1, [min y1 y2..])

tuplestoLine :: [(Int,Int)] -> Line
tuplestoLine ((x1,y1):(x2,y2):xs)
    | null xs = Line (Point x1 y1) (Point x2 y2)
    | otherwise = error "trying to parse a line with more than one starting and ending point"
tuplestoLine _ = error "trying to parse incorrect tuple of line"

parse :: String -> [Line]
parse = map (
    tuplestoLine
    . map ((\[x,y] -> (read x :: Int,read y :: Int)). splitOn ",". trim) . splitOn' "->")
    . lines

getMaxCoords :: [Line] -> (Int, Int)
getMaxCoords ls = (maximum xs, maximum ys)
    where
        (xs, ys) = unzip
            . map (\(Line (Point x1 y1) (Point x2 y2)) -> (max x1 x2,max y1 y2))
            $ ls

markCanvasIndex :: Num a => [[a]] -> Point -> [[a]]
markCanvasIndex c (Point x y) = insert2D (count+1) y x c
    where count = (c !! y) !! x

paintLine :: Canvas -> Line -> Canvas
paintLine c l = foo c (lineTrace l)
    where foo c []     = c
          foo c (x:xs) = foo (markCanvasIndex c x) xs

solve1 :: [Line] -> Int
solve1 ls = length
            . filter (>1)
            . concat
            . take (xmax+1)
            . map (take (ymax+1))
            . foo (repeat (repeat 0))
            $ ls
    where
        (xmax, ymax) = getMaxCoords ls
        foo :: Canvas -> [Line] -> Canvas
        foo c []     = c
        foo c (l:ls) = foo (paintLine c l) ls

solve2 :: [Line] -> Int
solve2 = solve1 . filter (\l -> isStraitLine l || isDiagonalLine l)

main :: IO ()
main = do
    file <- readFile "input.txt"
    putStrLn $ "Part 1:"++(show . solve1 . filter isStraitLine . parse $ file)
    putStrLn $ "Part 2:"++(show . solve2 . parse $ file)
