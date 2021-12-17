import           Data.Char       (isSpace)
import           Data.Function   (on)
import           Data.List       (dropWhileEnd, maximumBy, stripPrefix)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust)

-- Tried to find a smart way, gave up to the bruteforce method

type Vec = (Int,Int)

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

prepare :: String -> (Vec, Vec)
prepare = (\(x:y:_) -> (x,y))
        . map ((\(x:y:_) -> (read x, read y)) . splitOn ".." . drop 2)
        . splitOn ", "
        . trim
        . fromJust
        . stripPrefix "target area:"

step :: Vec -> Vec -> (Vec,Vec)
step (x,y) (vx,vy) = ((x', y'), (vx',vy'))
    where
        x' = x+vx
        y' = y+vy
        vy' = vy-1
        vx' = vx + case compare vx 0 of
                        GT -> -1
                        EQ -> 0
                        LT -> 1

willBeInside :: (Vec,Vec) -> Int -> Vec -> Vec -> (Vec, Int, Vec, Bool)
willBeInside f@((xmin,xmax),(ymin,ymax)) maxy p@(x,y) v@(vx,vy)
    | x > xmax || y < ymin = (p,maxy,(vx,vy+1),False)
    | x >= xmin && x <= xmax &&  y >= ymin && y <= ymax = (p,maxy,(vx,vy+1),True)
    | otherwise = uncurry (willBeInside f (max maxy (snd p))) (step p v)

solve1 :: (Vec, Vec) -> Int
solve1 f@((xmin,xmax),(ymin,ymax)) = max
    where startvs = [(vx,vy) | vy <- [0..200], vx <- [0..200]]
          max = snd' $ maximumBy (compare `on` snd') xs
          xs = filter frth
            $ map (\v -> let (pf,maxy, vf,b) = willBeInside f 0 (0,0) v in (v, maxy, vf, b)) startvs

snd' :: (a, b, c, d) -> b
snd' (_,x,_,_) = x
frth :: (a, b, c, d) -> d
frth (_,_,_,x) = x

solve2 ::  (Vec, Vec) -> Int
solve2 f@((xmin,xmax),(ymin,ymax))
    = length xs
    where
        startvs = [(vx,vy) | vy <- [-250..250], vx <- [0..250]]
        max = snd' $ maximumBy (compare `on` snd') xs
        xs = filter frth
            $ map (\v -> let (pf,maxy, vf,b) = willBeInside f 0 (0,0) v in (v, maxy, vf, b)) startvs

main :: IO ()
main = do
    file <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show . solve1 . prepare $ file)
    putStrLn $ "Part 2: " ++ (show . solve2 . prepare $ file)
