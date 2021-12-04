import           Data.Char       (isSpace)
import           Data.List       (find, transpose)
import           Data.List.Split (dropBlanks, dropDelims, oneOf, split, splitOn)
import           Data.Maybe      (fromJust)

type Square = (Bool, Int)

type Row = [Square]

type Table = [Row]

groupFive :: [a] -> [[a]]
groupFive [] = []
groupFive ls = take 5 ls : groupFive (drop 5 ls)

strsToInts :: [String] -> [Int]
strsToInts = map (\x -> read x :: Int)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | splits, removes the delimeter and any other delimeter element
--   that still is left in the list
splitOn' :: String -> String -> [String]
splitOn' delim = split (dropDelims . dropBlanks $ oneOf delim)

-- | groups five rows and converts elements to ints and zips together
--   with a boolean to indicate if the number is chosen or not.
getTables :: [String] -> [Table]
getTables =
  groupFive
    . map (zip (repeat False) . strsToInts . splitOn' " " . trim)
    . tail

prepare :: String -> ([Int], [Table])
prepare xs = (rands, tables)
  where
    rands = strsToInts . splitOn "," . head $ lns
    tables = getTables lns
    lns = splitOn' "\n" xs

mark :: Int -> Table -> Table
mark n = map (map(\(m, i) ->
    if i == n
    then (True, i)
    else (m, i)))

isAllMarked :: Row -> Bool
isAllMarked = all fst

isBingo :: Table -> Bool
isBingo t = isAnyRowsMarked || isAnyColsMarked
  where
    isAnyRowsMarked = any isAllMarked t
    isAnyColsMarked = any isAllMarked . transpose $ t

hasWinner :: [Table] -> Bool
hasWinner = any isBingo

removeWinner :: [Table] -> [Table]
removeWinner = filter (not . isBingo)

-- | Returns list of values of unmarked Squares
getUnmarked :: Table -> [Int]
getUnmarked =
  map snd
    . filter (not . fst)
    . concat

-- | after how many will this table have bingo?
winsAfter :: [Int] -> Table -> Int
winsAfter rands t = foo 0 rands t
  where
    foo :: Int -> [Int] -> Table -> Int
    foo _ [] _ = maxBound :: Int
    foo acc (r : rands) t
      | isBingo t = acc
      | otherwise = foo (acc + 1) rands (mark r t)

-- | play for n steps
play :: Int -> [Int] -> Table -> Table
play 0 _ t           = t
play _ [] t          = t
play n (r : rands) t = play (n -1) rands $ mark r t

solve1 :: [Int] -> [Table] -> Int
solve1 rands tables = sum unmarked * lastCalled
  where
    (t, n) = fromJust . find (\x -> snd x == minimum ns) $ zip tables ns
    ns = map (winsAfter rands) tables
    unmarked = getUnmarked . play n rands $ t
    lastCalled = rands !! (n -1)

solve2 :: [Int] -> [Table] -> Int
solve2 rands tables
  | n < length rands = rands !! (n -1) * sum unmarked
  | otherwise = error "no last winner found, maybe input is incorrect I'm not sure"
  where
    (t, n) = fromJust . find (\x -> snd x == maximum ns) $ zip tables ns
    ns = map (winsAfter rands) tables
    unmarked = getUnmarked . play n rands $ t

main :: IO ()
main = do
  file <- readFile "input.txt"
  putStrLn $ "Part 1: " ++ (show . uncurry solve1 . prepare $ file)
  putStrLn $ "Part 2: " ++ (show . uncurry solve2 . prepare $ file)
