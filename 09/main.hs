{-
Your eyes hurt from looking at this code?
Mine too, mine too...

counting my last days til' I can't continue AOC, its getting difficult for me now
 -}


import Data.Char
import qualified Data.Set as Set
import Data.List

prepare :: String -> [[Int]]
prepare = map (map digitToInt) . lines

isLow (rprev,r,rnext) (i,x) = x < left && x < right && x < up && x < down
    where left = if i < 1 then 9 else r !! (i-1)
          right = if i > length r - 2 then 9 else r !! (i+1)
          up = if null rprev then 9 else rprev !! i
          down = if null rnext then 9 else rnext !! i

toRowTrips rs = zip3 padded (drop 1 padded) (drop 2 padded)
    where padded = [] : rs ++ [[]]

lowests xs = map (\r3@(rp,r,rn) -> filter (isLow r3) (zip [0..] r)) (toRowTrips xs)

solve1
    = sum
    . map (\(_,x) -> x+1)
    . concat
    . lowests

isValidPoint xs p@(x,y)
    =  x >= 0
    && x < length xs
    && y >= 0
    && y < length (head xs)

neighbors xs p@(x,y)
    = Set.fromList
    $ filter (isValidPoint xs) [(x+1,y), (x-1,y), (x, y+1), (x,y-1)]

lowestPoints xs
    = concatMap (\(i,r3@(rp,r,rn)) -> map (\(c,_) -> (i,c))
    $ filter (isLow r3) (zip [0..] r)) (zip [0..]
    $ toRowTrips xs)

candidates xs visited 
    = Set.filter (\p@(x,y) -> xs!!x!!y /= 9 && Set.notMember p visited)
    . foldl Set.union Set.empty
    $ Set.map (neighbors xs) visited

getBasins xs p@(x,y) = find xs (Set.singleton p)
    where find xs visited
            | Set.null toVisit = visited
            | otherwise        = find xs (Set.union visited toVisit)
            where toVisit = candidates xs visited

solve2 rs
    = product
    . take 3
    . sortBy (flip compare)
    . map (Set.size . getBasins rs)
    $ lowestPoints rs

main = do
    file <- readFile "input.txt"
    putStrLn $ "Part 1: "++(show . solve1 . prepare $ file)
    putStrLn $ "Part 2: "++(show . solve2 . prepare $ file)

