

prepare = undefined

solve1 = id

solve2 = id


main :: IO ()
main = do
    file <- readFile "test_input.txt"
    putStr $ "part1"++show . solve1 . prepare $ file
    putStr $ "part 2:"++show . solve2 . prepare $ file

