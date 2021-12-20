-- Had to whip out http://learnyouahaskell.com/ for this one :)


import Control.Applicative ((<|>))
import Data.List (dropWhileEnd)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isNothing)

data Snum = Val Int | Pair {left :: Snum, right :: Snum} deriving (Ord, Eq)

instance Show Snum where
  show (Val n) = show n
  show (Pair sn1 sn2) = "[" ++ show sn1 ++ "," ++ show sn2 ++ "]"

data Crumb = LeftCrumb Snum | RightCrumb Snum deriving (Show, Eq)

type Breadcrumbs = [Crumb]

type Zipper = (Snum, Breadcrumbs)

topMost :: Zipper -> Maybe Zipper
topMost (t, []) = Just (t, [])
topMost z = goUp z >>= topMost

leftMost :: Zipper -> Maybe Zipper
leftMost z@(Val _, bs) = Just z
leftMost z = goLeft z >>= leftMost

rightMost :: Zipper -> Maybe Zipper
rightMost z@(Val _, bs) = Just z
rightMost z = goRight z >>= rightMost

goUp :: Zipper -> Maybe Zipper
goUp (t, LeftCrumb r : bs) = Just (Pair t r, bs)
goUp (t, RightCrumb l : bs) = Just (Pair l t, bs)
goUp (_, []) = Nothing

goLeft :: Zipper -> Maybe Zipper
goLeft z@(Val n, _) = Just z
goLeft (Pair l r, bs) = Just (l, LeftCrumb r : bs)

goRight :: Zipper -> Maybe Zipper
goRight z@(Val n, _) = Just z
goRight (Pair l r, bs) = Just (r, RightCrumb l : bs)

goUpWhileEq :: (Snum -> Snum) -> Snum -> Zipper -> Maybe Zipper
goUpWhileEq f prev z@(t, _) = if f t == prev then goUp z >>= goUpWhileEq f t else Just z

foo :: Zipper -> Maybe Zipper
foo z@(t, LeftCrumb _ : bs) = goUp z
foo z = goUp z >>= foo

goNext :: Zipper -> Maybe Zipper
goNext z@(_, []) = goRight z >>= leftMost
goNext z@(t@(Val _), _) = foo z
goNext z = goRight z >>= leftMost

foo' :: Zipper -> Maybe Zipper
foo' z@(t, RightCrumb _ : bs) = goUp z
foo' z = goUp z >>= foo'

goPrev :: Zipper -> Maybe Zipper
goPrev z@(_, []) = goLeft z >>= rightMost
goPrev z@(t@(Val _), _) = foo' z
goPrev z = goLeft z >>= rightMost

splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst delim xs = helper delim [] xs
  where
    helper _ acc [] = (acc, [])
    helper d acc (x : xs)
      | x == d = (acc, xs)
      | otherwise = helper d (acc ++ [x]) xs

getSnum :: String -> Snum
getSnum = fst . parseSnum

parseSnum :: String -> (Snum, String)
parseSnum [] = error "empty tree"
parseSnum ('[' : xs) = (Pair leftSnum rightSnum, rest')
  where
    (leftSnum, rest) = parseSnum xs
    (rightSnum, rest') = parseSnum rest
parseSnum (']' : xs) = parseSnum xs
parseSnum xs = (Val (read n'), rest)
  where
    (n, rest) = splitAtFirst ',' xs
    n' = dropWhileEnd (== ']') n

prepare :: String -> [Snum]
prepare = map getSnum . lines

leftMostPair :: Zipper -> Maybe Zipper
leftMostPair z = leftMost z >>= goUp

leftMostValPair :: Zipper -> Maybe Zipper
leftMostValPair z = leftMost z >>= nextValPair

nextPair :: Zipper -> Maybe Zipper
nextPair z = case goNext z of
  Just z'@(Pair _ _, _) -> Just z'
  Just z' -> nextPair z'
  Nothing -> Nothing

nextValPair :: Zipper -> Maybe Zipper
nextValPair z = case goNext z of
  Just z'@(Pair (Val _) (Val _), _) -> Just z'
  Just z' -> nextValPair z'
  Nothing -> Nothing

prevValPair :: Zipper -> Maybe Zipper
prevValPair z = case goPrev z of
  Just z'@(Pair (Val _) (Val _), _) -> Just z'
  Just z' -> prevValPair z'
  Nothing -> Nothing

modifyPair :: (Snum -> Snum) -> Zipper -> Maybe Zipper
modifyPair f (t, bs) = Just (f t, bs)

modifyVal :: (Int -> Int) -> Zipper -> Zipper
modifyVal f p@(Pair _ _, _) = p
modifyVal f (Val n, bs) = (Val (f n), bs)

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

goPrevVal :: Zipper -> Maybe Zipper
goPrevVal z = case goPrev z of
  Just z'@(Val _, _) -> Just z'
  Just z' -> goPrevVal z'
  Nothing -> Nothing

goNextVal :: Zipper -> Maybe Zipper
goNextVal z = case goNext z of
  Just z'@(Val _, _) -> Just z'
  Just z' -> goNextVal z'
  Nothing -> Nothing

explode :: Zipper -> Maybe Zipper
explode z = case nextValPair z of
  Just z'@(Pair (Val a) (Val b), bs) ->
    if length bs >= 4
      then
        if exploded /= z'
          then Just exploded
          else case explode exploded of
            Just z'' -> Just z''
            Nothing -> Just z'
      else let x = explode z' in if isNothing x then Just z' else x
    where
      Just prev' = modifyPair (const (Val 0)) z'
      left = goPrevVal prev'
      prev = case left of
        Just z'' -> modifyVal (+ a) z''
        Nothing -> prev'
      right = case left of
        Just x -> goNextVal prev >>= goNextVal
        Nothing -> goNextVal prev
      exploded = case right of
        Just z'' -> modifyVal (+ b) z''
        Nothing -> prev
  z' -> z'

split :: Zipper -> Maybe Zipper
split z@(Val n, bs)
  | n >= 10 = modifyPair (const (Pair (Val v) (Val (v + r)))) z
  | otherwise = case goNextVal z of
    Just z' -> if z' == z then Just z else split z' <|> Just z
    Nothing -> Just z
  where
    (v, r) = n `divMod` 2
split z = case goNextVal z of
  Just z' -> split z'
  Nothing -> Just z

add :: Snum -> Snum -> Snum
add a b = reduce $ Pair a b

reduce' :: Zipper -> Maybe Zipper
reduce' z = if splitted == Just z then splitted else splitted >>= reduce'
  where
    exploded = leftMost z >>= explode >>= topMost
    splitted =
      if exploded == Just z
        then exploded >>= leftMost >>= split >>= topMost
        else exploded

reduce :: Snum -> Snum
reduce snum = fst res
  where
    Just res = reduce' (snum, [])

magnitude :: Snum -> Int
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r
magnitude (Val n) = n

solve :: Map (Snum, Snum) Snum -> Snum -> [Snum] -> (Snum, Map (Snum, Snum) Snum)
solve m acc [] = (acc, m)
solve m acc (s : snums) =
  let (res, m') = foo (acc, m) s
   in solve m' res snums
  where
    foo (acc', m') s' = case M.lookup (acc', s') m' of
      Just x -> (x, m')
      Nothing -> (res, M.insert (acc', s') res m')
      where
        res = add acc' s'

solve1 :: [Snum] -> Int
solve1 = magnitude . foldl1 (\sn1 sn2 -> reduce $ add sn1 sn2)

bar :: Map (Snum, Snum) Snum -> [Snum] -> [[Snum]] -> [Snum]
bar _ acc [] = acc
bar m acc (p : ps) = let (res, m') = solve m (head p) (tail p) in bar m' (res : acc) ps

solve2 :: [Snum] -> Int
solve2 xs = maximum . map magnitude $ [add x y | x <- xs, y <- xs]

main :: IO ()
main = do
  file <- readFile "input.txt"
  putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
  putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)
