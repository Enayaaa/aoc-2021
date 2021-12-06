import Data.List.Split
import Data.List
import Debug.Trace
import Data.Array
import qualified Data.Map as M

step :: Int -> [Int]
step 0 = [6, 8]
step x = [x-1]

count e = length . filter (==e)

--update :: [Int] -> [Int]
update m fs = concatMap step (nub fs) --(trace ("hello "++show (length fs)) fs)
    where xs = map (`count` fs) fs

prepare :: String -> [Int]
prepare = map read . splitOn ","

babiesIn x = [x+1+7*i | i <- [0..]]

decendentsIn :: Array (Int,Int) (Maybe Int) -> Int -> Int -> (Array (Int,Int) (Maybe Int), Int)
decendentsIn a n x =
    case a ! (x,n) of
    Nothing -> (a // [((x,n), Just children)], children)
    Just res -> (a, res)
    where xs = takeWhile (<=n) $ babiesIn x
          children = length xs + sum (map (\d -> let (_,res) = decendentsIn a (n-d) 8 in res) xs)

solve1 :: Int -> [Int] -> Int
solve1 n fs = length . last . take (n+1) . iterate (update m) $ fs
    where m c= M.fromList $ zip [0..8] (repeat 0)

solve2 a n [] = 0
solve2 a n (f:fs) = 1+res + solve2 a' n fs
    where
        (a',res) = decendentsIn a n f

        m = M.fromList $ zip [0..8] (repeat Nothing)
        --xs = map (\x -> let (_,res) = decendentsIn a n x in res) fs

--main :: IO ()
main = do
    file <- readFile "input.txt"
    --putStrLn $ "part 1:"++(show . solve1 n . prepare $ file)
    putStrLn $ "part 2:"++(show . solve2 n . prepare $ file)
    where
          n = 256
    --return (prepare file)
