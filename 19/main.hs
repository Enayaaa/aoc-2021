{-# LANGUAGE FlexibleContexts #-}
import           Data.Function   (on)
import           Data.List       (sort, sortBy, transpose, (\\))
import           Data.List.Split (splitOn)
import           Data.Maybe      (isJust)
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

vecSub :: Vec -> Vec -> Vec
vecSub = zipWith (-)

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

distancesFrom x xs = sortBy (\a b -> compare (snd a) (snd b)) $ map (\p -> ((x,p),distance x p)) xs

--sortByDistance = sortBy (compare `on` snd)

count = 12

{-
[[404,-588,-901]], fromList (
fromList [
[326,483,1331],
[378,391,1186],
[404,-760,402],
[430,301,1323],
[447,-806,410],
[464,-828,1234],
[528,-643,409],
[547,-785,1267],
[566,-955,1193],
[580,162,-57],
[618,133,-85],
[720,196,-103],
[1038,-250,694],
[1455,-978,261],
[1461,-748,1517],
[1469,-1086,174],
[1473,618,1401],
[1488,249,287],
[1497,665,1595],
[1524,216,293],
[1562,-802,1424],
[1593,224,229],
[1599,583,1498],
[1609,-1075,213],
[1633,-762,1593]]

,fromList [
[-892,524,684],
[-876,649,763],
[-838,591,734],
[-789,900,-551],
[-689,845,-530],
[-661,-816,-575],
[-618,-824,-621],
[-584,868,-557],
[-537,-823,-458],
[-485,-357,347],
[-447,-329,318],
[-345,-311,381],
[7,-33,-71],
[390,-675,-793],
[404,-588,-901],
[423,-701,434],
[443,580,662],
[455,729,728],
[459,-707,401],
[474,580,667],
[528,-643,409],
[544,-627,-890],
[553,345,-567],
[564,392,-477],
[630,319,-379]]
)
-}


checkIntersection as bs offset = traceShow (translated, origin) intersection
{-
    | S.size intersection >= count = Just offset
    | otherwise = Nothing
    -}
    where
        translated = S.fromList $ map (vecAdd offset) bs
        origin = S.fromList as
        intersection = S.intersection origin translated

pairup as bs = map (checkIntersection as bs) offsets
    where
        offsets = zipWith vecSub as bs

test xs = map (pairup as) (orientAll bs)
    where
        as = head xs
        bs = head $ tail xs



main :: IO ()
main = do
    file <- readFile "test_input.txt"
    --file <- readFile "input.txt"
    --file <- readFile "sample.txt"

    --putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
    print (test . prepare $ file)
    --putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)








