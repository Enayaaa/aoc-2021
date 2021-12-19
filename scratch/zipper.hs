
data Tree a = Empty | Node {val :: a,  left :: Tree a, right :: Tree a} deriving (Show, Eq)
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a)

topMost :: Zipper a -> Maybe (Zipper a)
topMost (t,[]) = Just (t,[])
topMost z = goUp z >>= topMost

leftMost :: Zipper a -> Maybe (Zipper a)
leftMost z@(Node a Empty _, bs) = Just z
leftMost z = goLeft z >>= leftMost

rightMost :: Zipper a -> Maybe (Zipper a)
rightMost z@(Node a _ Empty, bs) = Just z
rightMost z = goRight z >>= rightMost

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

goUpWhile :: (Tree a -> Bool) -> Zipper a -> Maybe (Zipper a)
goUpWhile f z@(n, _)
    | f n = goUp z >>= goUpWhile f
    | otherwise = Just z

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft z@(Node x Empty r, bs) = Just z
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight z@(Node x l Empty, bs) = Just z
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (Empty, _) = Nothing

goNext :: Eq a => Zipper a -> Maybe (Zipper a)
goNext z@(_,[]) = goRight z >>= leftMost
goNext z@(t@(Node _ _ Empty), _) = goUp z >>= goUpWhileEq right t
goNext z = goRight z >>= leftMost

goUpWhileEq :: Eq a => (Tree a -> Tree a) -> Tree a -> Zipper a -> Maybe (Zipper a)
goUpWhileEq f prev z@(t,_) = if f t == prev then goUp z >>= goUpWhileEq f t else Just z

goPrev :: Eq a => Zipper a -> Maybe (Zipper a)
goPrev z@(_,[]) = goLeft z >>= rightMost
goPrev z@(t@(Node _ _ Empty), _) = goUp z >>= goUpWhileEq left t
goPrev z = goLeft z >>= rightMost

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

freeZip :: Zipper Char
freeZip = (freeTree, [])

