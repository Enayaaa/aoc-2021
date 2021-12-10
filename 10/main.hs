import Data.Maybe (mapMaybe)

newtype Stack a = Stack [a] deriving (Show, Eq)

emptyStack :: Stack a
emptyStack = Stack []

stackPush :: a -> Stack a -> Stack a
stackPush x (Stack s) = Stack (x:s)

stackPop :: Stack a -> (Maybe a, Stack a)
stackPop (Stack [])     = (Nothing, Stack [])
stackPop (Stack (x:xs)) = (Just x, Stack xs)

stackPeek :: Stack a -> (Maybe a, Stack a)
stackPeek (Stack [])     = (Nothing, Stack [])
stackPeek (Stack s@(x:xs)) = (Just x, Stack s)

isClosing :: Char -> Bool
isClosing x = x `elem` ")]}>"

closingPair :: Char -> Char
closingPair '(' = ')'
closingPair '[' = ']'
closingPair '{' = '}'
closingPair '<' = '>'
closingPair _ = error "Unknown opening bracket type"

getFirstErrorous :: Stack Char -> [Char] -> Maybe Char
getFirstErrorous s [] = Nothing
getFirstErrorous s (x:xs)
    | isClosing x = case stackPop s of
            (Just y, s) -> if x == closingPair y then getFirstErrorous s xs else Just x
            __          -> Just x
    | otherwise = getFirstErrorous (stackPush x s) xs

syntaxErrorScore :: Char -> Int
syntaxErrorScore ')' = 3
syntaxErrorScore ']' = 57
syntaxErrorScore '}' = 1197
syntaxErrorScore '>' = 25137
syntaxErrorScore x = error $ "Error score not found for illegal item `"++show x++"`."

solve1 :: [String] -> Int
solve1
    = sum
    . map syntaxErrorScore
    . mapMaybe (getFirstErrorous emptyStack)

getCompletingEnd :: Stack Char -> [Char] -> Maybe [Char]
getCompletingEnd (Stack s) [] = Just $ map closingPair s
getCompletingEnd s (x:xs)
    | isClosing x = case stackPop s of
            (Just y, s) -> if x == closingPair y then getCompletingEnd s xs else Nothing
            __          -> undefined
    | otherwise = getCompletingEnd (stackPush x s) xs

pointValue :: Char -> Int
pointValue ')' = 1
pointValue ']' = 2
pointValue '}' = 3
pointValue '>' = 4
pointValue x = error $ "Error, pointValue not found for illegal item `"++show x++"`."

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
    where smallerSorted = sort [a | a <- xs, a <= x]
          biggerSorted = sort [a | a <- xs, a > x]

getMiddleElem :: [a] -> a
getMiddleElem xs = xs !! (length xs `div` 2)

solve2 :: [String] -> Int
solve2
    = getMiddleElem
    . sort
    . map (foldl (\sum x -> sum*5 + pointValue x) 0)
    . mapMaybe (getCompletingEnd emptyStack)

main :: IO ()
main = do
    file <- readFile "input.txt"
    putStrLn $ "Part 1: "++(show . solve1 . lines $ file)
    putStrLn $ "Part 2: "++(show . solve2 . lines $ file)
