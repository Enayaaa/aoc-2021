import Data.Char (isSpace, digitToInt)
import Data.Bifunctor (bimap)
import Debug.Trace
import Data.List

data Packet = Literal {version:: Int, value :: Int}
            | OP {version :: Int, op :: Operator, subpackets :: [Packet]} deriving (Show)
data Operator = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo deriving (Show, Eq)


trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

takeBy :: Int -> [a] -> [[a]]
takeBy _ [] = []
takeBy n ls = take n ls : takeBy n (drop n ls)

toDec = foldl ((+) . (*2)) 0 . map digitToInt

toBin x = case x of
    '0' -> "0000"
    '1' -> "0001"
    '2' -> "0010"
    '3' -> "0011"
    '4' -> "0100"
    '5' -> "0101"
    '6' -> "0110"
    '7' -> "0111"
    '8' -> "1000"
    '9' -> "1001"
    'A' -> "1010"
    'B' -> "1011"
    'C' -> "1100"
    'D' -> "1101"
    'E' -> "1110"
    'F' -> "1111"
    _ -> error $ "unknown hex digit `"++show x++"`"

getSubPacketsWithBits n bits
    | n < 1 = ([], bits)
    | otherwise = (subp:xs, left)
    where
        (subp, threwAway) = toPacket bits
        (xs, left) = getSubPacketsWithBits (n-(length bits-length threwAway)) threwAway

getSubPacketsWithLen n bits
    | n < 1 = ([], bits)
    | otherwise = (subp : xs, left)
    where
        (subp, threwAway) = toPacket bits
        (xs, left) = getSubPacketsWithLen (n-1) threwAway


toPacket :: String -> (Packet, String)
toPacket bits = if null bits then error "something went wrong" else case pid of
    4 -> (Literal ver (toDec $ concatMap (drop 1) groups), concat junk)
    n -> (OP ver (toOp n) subpackets, after)
    where
        (ver, pid) = bimap toDec toDec $ splitAt 3 header
        (header, body) = splitAt 6 bits
        -- Literal
        (groups,junk)
            = (\(ones,r) -> (ones++[head r], tail r))
            $ span ((=='1') . head)
            $ takeBy 5 body
        -- OPPacket
        (lengthType, rest) = splitAt 1 body
        (countStr, rest') = case toDec lengthType of
            0 -> splitAt 15 rest
            1 -> splitAt 11 rest
            _ -> error "unreachable"
        count = toDec countStr
        (subpackets, after) = case toDec lengthType of
            0 -> getSubPacketsWithBits count rest'
            1 -> getSubPacketsWithLen count rest'
            _ -> error "unreachable"


prepare input = fst . toPacket $ concatMap toBin . trim $ input

getVersionsSum :: Packet -> Int
getVersionsSum (Literal v _) = v
getVersionsSum (OP v _ subs) = v + sum (map getVersionsSum subs)

solve1 = getVersionsSum

toOp 0 = Sum
toOp 1 = Product
toOp 2 = Minimum
toOp 3 = Maximum
toOp 5 = GreaterThan
toOp 6 = LessThan
toOp 7 = EqualTo
toOp _ = error "Unexpected Operator ID"

solve2 (Literal v n) = n
solve2 (OP v op xs) = case op of
    Sum -> sum $ map solve2 xs
    Product -> product $ map solve2 xs
    Minimum -> minimum $ map solve2 xs
    Maximum -> maximum $ map solve2 xs
    GreaterThan -> if solve2 (head xs) > solve2 (xs !! 1) then 1 else 0
    LessThan -> if solve2 (head xs) < solve2 (xs !! 1) then 1 else 0
    EqualTo -> if solve2 (head xs) == solve2 (xs !! 1) then 1 else 0

main = do
    file <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show . solve1 . prepare $ file)
    putStrLn $ "Part 2: " ++ (show . solve2 . prepare $ file)

