import           Data.Char       (isSpace)
import           Data.List       (sort, (\\))
import           Data.List.Split (splitOn)
import qualified Data.Map        as M (Map, fromList, (!))
import           Data.Maybe      (fromJust, isJust)

trim :: String -> String
trim = reverse . clean . reverse . clean
    where clean = dropWhile isSpace

type Segment = String
type Row = ([Segment],[Segment])

prepare :: String -> [Row]
prepare = map ((\(x:y:_) -> (x,y))  . map (splitOn " " . trim) . splitOn "|"). lines

segToDigit :: Segment -> Maybe Int
segToDigit x = case length x of
                2 -> Just 1
                3 -> Just 7
                4 -> Just 4
                7 -> Just 8
                _ -> case sort x of
                        "abcefg"  -> Just 0
                        "cf"      -> Just 1
                        "acdeg"   -> Just 2
                        "acdfg"   -> Just 3
                        "bcdf"    -> Just 4
                        "abdfg"   -> Just 5
                        "abdefg"  -> Just 6
                        "acf"     -> Just 7
                        "abcdefg" -> Just 8
                        "abcdfg"  -> Just 9
                        _         -> Nothing

isEasy :: Segment -> Bool
isEasy x = case segToDigit x of
            Just d  -> d `elem` [1,4,7,8]
            Nothing -> False

solve1 :: [Row] -> Int
solve1 = length . concatMap (filter isEasy . snd)

deduce :: (Ord k, Ord a, Num a) => [(Maybe a, [k])] -> M.Map k Char
deduce xs = M.fromList $ zip [segA,segB,segC,segD,segE,segF,segG] ['a'..'g']
    where m = M.fromList xs
          segA = head $ segs7 \\ segs1
          segB = head $ segs9 \\ segs3
          segC = head $ segs1 \\ segs6
          segD = head $ filter (`elem` segs4) segs3 \\ segs7
          segE = head $ segs8 \\ segs9
          segF = head $ segs1 \\ [segC]
          segG = head $ filter (`notElem` segs4) segs3 \\ segs7
          segs1 = m M.! Just 1
          segs3 = snd . head . filter ((==2) . length . (\\ segs7) . snd) $ withLen5
          segs4 = m M.! Just 4
          segs6 = snd . head . filter (elem False . (\x -> map (`elem` x) segs1) . snd) $ withLen6
          segs9 = snd . head . filter ( (==1) . length . (\\ segs3). snd) $ withLen6
          segs7 = m M.! Just 7
          segs8 = m M.! Just 8
          withLen6 = filter ((==6) . length . snd) xs
          withLen5 = filter ((==5) . length . snd) xs

digitsToNum :: Num a => [a] -> a
digitsToNum []     = 0
digitsToNum (x:xs) = x*product (replicate (length xs) 10)+digitsToNum xs

decode :: (Ord k, Ord a, Num a) => [[k]] -> [(Maybe a, [k])] -> Int
decode ns xs = digitsToNum $ map (fromJust . segToDigit . map (segsMap M.!)) ns
    where
        segsMap = deduce xs

solve2 :: [Row] -> Int
solve2 xs = sum $ map (\(xs, ns) -> decode ns . flip zip xs $ map segToDigit xs) xs

main :: IO ()
main = do
    file <- readFile "input.txt"
    putStrLn $ "Part 1: "++ (show . solve1 . prepare $ file)
    putStrLn $ "Part 2: "++ (show . solve2 . prepare $ file)
