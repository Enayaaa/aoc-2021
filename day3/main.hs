import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)

toDigitArr :: [Char] -> [Int]
toDigitArr = map digitToInt

invert :: [Int] -> [Int]
invert = map (1 -)

toDec :: [Int] -> Int
toDec seq = foo 0 (reverse seq)
  where
    foo :: Int -> [Int] -> Int
    foo _ [] = 0
    foo sh (x : xs) = product (replicate sh 2) * x + foo (sh + 1) xs

mostCommons :: (Ord a, Num a, Num b, Foldable t) => a -> t [a] -> [b]
mostCommons len = map (\x -> if len - x > x then 0 else 1) . foldl (zipWith (+)) (repeat 0)

leastCommons :: Int -> [[Int]] -> [Int]
leastCommons = (invert .) . mostCommons

solve1 :: [String] -> Int
solve1 input = toDec gamma * toDec epsilon
  where
    gamma =
      mostCommons (length input)
        . map toDigitArr
        $ input
    epsilon = invert gamma

solve2 :: [String] -> Int
solve2 input = oxyGen * co2Gen
  where
    inps = map toDigitArr input
    convert = toDec . head
    oxyGen = convert $ filterF mostCommons 0 inps
    co2Gen = convert $ filterF leastCommons 0 inps

    filterF _ _ [a] = [a]
    filterF f pos xs
      | pos >= 0 || pos < max (length xs) (length filtering) =
        filterF f (pos + 1) . filter (\x -> filtering !! pos == x !! pos) $ xs
      | otherwise = xs
      where
        filtering = f (length xs) xs

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let input = lines contents
  putStrLn $ "part 1: " ++ show (solve1 input)
  putStrLn $ "part 2: " ++ show (solve2 input)
