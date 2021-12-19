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

reduce snum = if updated then reduce res else res
    where
        (res,updated) = reduce' snum

        reduce' (Val x) = (Val x, False)
        reduce' (Pair left right) = case actionLeft of
            Explode n1 n2 -> (Pair left' (addToLeftMost n2 right), True)
            Split -> (Pair left' right, True)
            Zzz -> case actionRight of
                Explode n1 n2 -> (Pair (addToRightMost n1 left) right', True)
                Split -> (Pair left right', True)
                Zzz -> (Pair left right, False)
            where (left', actionLeft, _) = helper 2 left
                  (right', actionRight, _) = helper 2 right

        helper :: Int -> Snum -> (Snum, Action, Bool)
        helper 4 (Pair (Val v1) (Val v2)) = (Val 0, Explode v1 v2, True)
        helper n sn@(Val x) = if x >= 10
            then (Pair (Val v) (Val (v+r)), Zzz, True)
            else (sn, Zzz, False)
            where (v, r) = x `divMod` 2
        helper n (Pair sn1 sn2) = trace "hello" (Pair leftTree rightTree, action, updated1 || updated2)
            where
                (left, actionLeft, updated1) = helper (n+1) sn1
                (right, actionRight, updated2) = helper (n+1) sn2
                (leftTree, rightTree, action) = case actionLeft of
                    Explode n1 n2 -> (left, addToLeftMost n2 sn2, actionRight)
                    Split -> (left, sn2, actionRight)
                    Zzz -> case actionRight of
                        Explode n1 n2 -> (left, addToLeftMost n2 right, actionLeft)
                        Split -> (left, sn2, actionLeft)
                        Zzz -> (sn1,sn2,Zzz)

solve1 = reduce . head--foldl1 (\sn1 sn2 -> reduce $ add sn1 sn2)


solve2 = id

main = do
    file <- readFile "test_input.txt"
    --let file = "[[[[0,7],4],[15,[0,13]]],[1,1]]"
    --let file = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
    --file <- readFile "input.txt"
    --let file = "[[[[0,7],4],[15,[0,13]]]\n[1,1]]"
    --let file = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
    --let file = "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]"
    let file = "[[[[[9,8],1],2],3],4]"
    putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
    --putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)
