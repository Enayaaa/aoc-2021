{-# LANGUAGE FlexibleContexts #-}
import           Data.Function   (on)
import           Data.List       (sort, sortBy, transpose, (\\))
import           Data.List.Split (splitOn)
import           Data.Maybe      ()
import qualified Data.Set        as S
import           Debug.Trace

type Vec = [Int]
type Matrix3 = [Vec]

parsePoint :: String -> Vec
parsePoint = map read . splitOn ","

prepare :: String -> [[Vec]]
prepare = map (map parsePoint . tail . lines) . splitOn "\n\n"

vecAdd :: Vec -> Vec -> Vec
vecAdd = zipWith (+)

vecTimes :: Int -> Vec -> Vec
vecTimes c = map (*c)

dotProd v1 v2 = sum $ zipWith (*) (map fromIntegral v1) v2

matrixProd m v = map (round . dotProd v) (transpose m)

rotateX theta = matrixProd [[1,0,0], [0, cos theta, sin theta], [0, -sin theta, cos theta]]
rotateY theta = matrixProd [[cos theta, 0, -sin theta], [0,1,0], [sin theta, 0, cos theta]]
rotateZ theta = matrixProd [[cos theta, sin theta, 0], [-sin theta, cos theta, 0], [0,0,1]]

orientations :: Vec -> [Vec]
orientations p = [rotateZ theta x | x <- orients, theta <- thetas]
    where thetas = [0,pi/2.0,pi,3.0*pi/2]
          orients = map (`rotateY` p) thetas ++ [rotateX (pi/2) p, rotateX (-pi/2) p]

orientAll :: [Vec] -> [[Vec]]
orientAll ps = map (`map` ps) transforms
    where
        thetas = [0,pi/2.0,pi,3.0*pi/2]
        rotations = [id, rotateY (pi/2), rotateY pi, rotateY (3*pi/2), rotateX (pi/2), rotateX (-pi/2)]
        transforms = [rotateZ theta . f | f <- rotations, theta <- thetas]


distance a b = sqrt . sum . map (**2) $ zipWith (-) (map fromIntegral b) (map fromIntegral a)
magnitude = distance (repeat 0)

translateBy = zipWith (+)
translateAllBy vec = map (translateBy vec)

--solve1 :: [[Vec]] -> [[[Vec]]]
solve1 xs = foo (head pairs)
    where
        pairs = [(a,b) | a <- xs, b <- xs, a /= b]

foo (as,bs) = map sort $ filter (not . null) res
    where
        res = map (\b -> map (\s -> getUniques s (as\\[s]) b) as) (orientAll bs)

getUniques s as bs = if null pairs then [] else absoluteB
    where
        absoluteB = map (zipWith (+) tr) bs
        tr = zipWith (-) (fst p1) (fst p2)
        (p1,p2) = head pairs
        pairs
            = concat
            . filter ((>=11) . length)
            $ map (takeWhile (\(a,b) -> snd a == snd b) . zip pq . (\x -> distancesFrom x (bs \\ [x]))) bs
        pq = distancesFrom s as
        distancesFrom x xs = sortBy (\a b -> compare (snd a) (snd b))
            $ map (\p -> (p,distance x p)) xs

solve2 = id

showRow c xs = putStrLn (c : ':' : ' ' : unwords (map show xs))
showlist xxs c = mapM_ (showRow c) xxs

















takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ []     = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []


--insert e@((a,b),d) s = if S.notMember e S && S.notMember




distancesFrom x xs = sortBy (\a b -> compare (snd a) (snd b)) $ map (\p -> ((x,p),distance x p)) xs

sortByDistance = sortBy (compare `on` snd)

count = 11


getBeaconCoords as bs = head matches--if null matches then Nothing else Just sc
    where
        sc = zipWith (+) (vecTimes (-1) b) a
        (((a,_),_),((b,_),_)) = head $ last matches
        matches = filter (not . null) $ map (takeWhile (\(a,b) -> snd b == snd a)) ds
        ds = [zip (foo a (as\\[a])) (foo b (bs\\[b])) | a <- as, b <-bs]
        foo x xs = sortByDistance $ distancesFrom x xs

test xs = map (getBeaconCoords as) (orientAll bs)
    where
        as = head xs
        bs = head $ tail xs





main :: IO ()
main = do
    file <- readFile "test_input.txt"
    --file <- readFile "input.txt"
    --let file = "
    --file <- readFile "sample.txt"
    --let res = solve1 . prepare $ file
    --showlist res 'a'
    --putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
    print (test . prepare $ file)
    --putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)







