
type Pos = (Int, Int)
type PosAim = (Int, Int, Int)

solve1 :: [String] -> Int
solve1 xs = let (x,y) = update (0,0) xs in x*y
    where
        update :: Pos -> [String] -> Pos
        update pos [] = pos
        update (x,y) (a:as) = update (x+dx, y+dy) as
            where
                (dx, dy) = (parse (words a)) 
        
        parse :: [String] -> Pos
        parse (command:arg:xs)
            | command == "forward" = (read arg :: Int, 0)
            | command == "down" = (0, read arg :: Int)
            | command == "up" = (0, - (read arg ::Int))
            | otherwise = error "Unknown commands found, input not correct"

solve2 :: [String] -> Int
solve2 xs = let (x,y,_) = update (0,0,0) xs in x*y
    where
        update :: PosAim -> [String] -> PosAim
        update posaim [] = posaim
        update (x,y,aim) (a:as) = update (x+dx, y+dy, aim+daim) as
            where
                (dx, dy, daim) = (parse (words a) aim) 
        
        parse :: [String] -> Int -> PosAim
        parse (command:arg:xs) aim
            | command == "forward" = (x, aim*x, 0)
            | command == "down" = (0, 0, x)
            | command == "up" = (0, 0, -x)
            | otherwise = error "Unknown commands found, input not correct"
            where
                x = read arg :: Int

main = do
    contents <- readFile "input.txt"
    let input = lines contents
    putStrLn $ show $ solve2 input
