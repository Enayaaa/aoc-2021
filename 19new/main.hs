import           Control.Monad   (zipWithM)
import           Data.List       (intercalate, transpose)
import           Data.List.Split (splitOn)
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as S

type Vec = [Int]
type Point = Vec
type Scanner = Set Point

parsePoint :: String -> Vec
parsePoint = map read . splitOn ","

prepare :: String -> Set Scanner
prepare input = foldl (flip S.insert) S.empty xs
    where
    xs = map (S.fromList . map parsePoint . tail . lines) $ splitOn "\n\n" input

showScanners :: [Scanner] -> IO [()]
showScanners = zipWithM showScanner [0..]

showScanner :: Int -> Scanner -> IO ()
showScanner i s = do
    putStrLn $ "--- scanner "++show i++" ---\n" ++concatMap ((++"\n") . show) s

vecAdd :: Vec -> Vec -> Vec
vecAdd = zipWith (+)
vecSub :: Vec -> Vec -> Vec
vecSub = zipWith (-)

matrixProd :: [Vec] -> [Vec] -> [Vec]
matrixProd xs ys = transpose [[sum $ zipWith (*) a b | a <- xs] | b <- transpose ys]

prod :: [Vec] -> Vec -> Vec
prod m v = head . transpose $ matrixProd m (transpose [v])

rZ90 :: Vec -> Vec
rZ90 = prod [[0,-1,0],[1,0,0],[0,0,1]]
rY90 :: Vec -> Vec
rY90 = prod [[0,0,1],[0,1,0],[-1,0,0]]
rX90 :: Vec -> Vec
rX90 = prod [[1,0,0],[0,0,-1],[0,1,0]]

orientations :: [Vec -> Vec]
orientations = [id , rY90, rY90 . rY90, rY90 . rY90 . rY90, rX90, rX90 . rX90 . rX90]

transformations :: [Vec -> Vec]
transformations = [f . o | o <- orientations, f <- [id, rZ90, rZ90 . rZ90, rZ90 . rZ90 . rZ90]]

allOrients :: Scanner -> [Scanner]
allOrients s = map (\f -> S.map f s) transformations

offset :: Vec -> Vec -> Vec
offset u v = u `vecSub` v

transform :: Vec -> Scanner -> Scanner
transform offset = S.map (vecAdd offset)

-- Assuming p is a beacon they have in common
getOffsets :: Point -> Scanner -> [Vec]
getOffsets p = S.toList . S.map (offset p)

guessOffset :: Scanner -> Scanner -> Maybe (Vec, Scanner)
guessOffset a b = try $ concatMap (`getOffsets` b) a
    where
    try [] = Nothing
    try (o':os)
        | S.size (S.intersection a transformed) >= 12 = Just (o',b)
        | otherwise = try os
        where
        transformed = transform o' b

guessOrientation :: Set Point -> Scanner -> Maybe (Vec, Scanner)
guessOrientation a b = try $ allOrients b
    where
    try [] = Nothing
    try (x:xs) = case guessOffset a x of
        Nothing -> try xs
        Just b' -> Just b'

collectBeacons :: Set Point -> [Vec] -> Set Scanner -> (Set Point, [Vec])
collectBeacons x known xs = let (o,s) = helper $ mapMaybe (guessOrientation x) (S.toList xs) in
    if length known < S.size xs
    then collectBeacons (S.union x (transform o s)) (o:known) xs
    else (x,known)
    where
    helper [] = error "not found"
    helper ((o,s):os)
        | o `elem` known = helper os
        | otherwise = (o,s)

solve1 :: Set Scanner -> Int
solve1 xs = S.size . fst $ collectBeacons (S.elemAt 0 xs) [[0,0,0]] xs

manhattan :: Vec -> Vec -> Int
manhattan a b = sum $ map abs $ zipWith (-) a b

solve2 :: Set Scanner -> Int
solve2 xs = maximum [manhattan a b | a <- ds, b <- ds]
    where
    ds = snd $ collectBeacons (S.elemAt 0 xs) [[0,0,0]] xs

main :: IO ()
main = do
    --file <- readFile "test_input.txt"
    file <- readFile "input.txt"
    putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
    putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)
