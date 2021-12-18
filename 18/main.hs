import Debug.Trace
import Data.List

data Snum = Val Int | Pair Snum Snum deriving (Show)
data Action = Zzz | Explode Int Int | Split deriving (Show)

splitAtFirst :: Eq a => a -> [a] -> ([a],[a])
splitAtFirst delim xs = helper delim [] xs
    where
        helper _ acc [] = (acc,[])
        helper d acc (x:xs)
            | x == d = (acc, xs)
            | otherwise = helper d (acc++[x]) xs

getTree = fst . parseTree

parseTree [] = error "empty tree"
parseTree ('[':xs) = (Pair leftTree rightTree, rest')
    where
        (leftTree, rest) = parseTree xs
        (rightTree, rest') = parseTree rest
parseTree (']':xs) = parseTree xs
parseTree xs = (Val (read n'), rest)
    where
        (n,rest) = splitAtFirst ',' xs
        n' = dropWhileEnd (==']') n

prepare = map getTree . lines

add = Pair

addToRightMost n (Val x) = Val (x+n)
addToRightMost n (Pair sn1 sn2) = Pair sn1 (addToRightMost n sn2)

addToLeftMost n (Val x) = Val (x+n)
addToLeftMost n (Pair sn1 sn2) = Pair (addToLeftMost n sn1) sn2

hasLeafs (Pair (Val _) (Val _)) = True
hasLeafs _ = False

isLeaf (Val _) = True
isLeaf _ = False

getRightMost (Val x) = x
getRightMost (Pair sn1 sn2) = getRightMost sn2

reduce snum = res -- if updated then reduce res else res
    where
        (res,_,updated) = reduce' snum

        reduce' snum = helper 0 snum

        helper :: Int -> Snum -> (Snum, Action, Bool)
        helper 4 (Pair (Val v1) (Val v2)) = (Val 0, Explode v1 v2, True)
        helper n sn@(Val x) = if x >= 10
            then (Pair (Val v) (Val (v+r)), Split, True)
            else (sn, Zzz, False)
            where (v, r) = x `divMod` 2
        helper n (Pair left right)
            | updatedLeft = case actionLeft of
                Explode n1 n2 -> if isLeaf left
                    then (Pair left' (addToLeftMost n2 right'), actionLeft, False)
                    else (Pair left' right, actionLeft, False)
                a -> (Pair left' right, a, False)
            | updatedRight = case actionRight of
                Explode n1 n2 -> if isLeaf right
                    then (Pair (addToRightMost n1 left') right', actionRight, False)
                    else (Pair left right', actionRight, False)
                a -> (Pair left right', a, False)
            | otherwise = case actionLeft of
                Explode n1 n2 -> (Pair left' (addToLeftMost n2 right), actionLeft, False)
                Split -> (Pair left' right, actionLeft, False)
                Zzz -> case actionRight of
                    Explode n1 n2 -> (Pair (addToRightMost n1 left) right', actionRight, False)
                    Split -> (Pair left right', actionRight, False)
                    Zzz -> (Pair left right, actionRight, False)
            where
                (left', actionLeft, updatedLeft) = helper (n+1) left
                (right', actionRight, updatedRight) = helper (n+1) right


solve1 snums = foldl (\sn1 sn2 -> reduce $ add sn1 sn2) (reduce $ head snums) (tail snums)

{-
                       /\
    *           [0,0]      [9,5]
                /\            /\
               0  [4,5]  [4,5] [2,6]


-}
solve2 = id

main = do
    file <- readFile "test_input.txt"
    file <- readFile "sample2.txt"
    --file <- readFile "sample1.txt"
    --file <- readFile "input.txt"
    --let file = "[[[[0,7],4],[15,[0,13]]]\n[1,1]]"
    --let file = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
    --let file = "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]"
    putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
    --putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)
