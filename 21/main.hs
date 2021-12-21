import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as M

type DeterDice = Int
data Player = Player {score :: Int, pos :: Int} deriving (Show, Ord, Eq)
data Game = Game DeterDice Player Player deriving Show

prepare :: String -> [Int]
prepare = map (read . last . splitOn ": ") . lines

roll3 :: DeterDice -> (Int, DeterDice)
roll3 d = let s = sum $ map (+ d) [0..2] in
    (s, d+3)

isWinnerDeter :: Player -> Bool
isWinnerDeter (Player s p) = s >= 1000

moveDeter :: DeterDice -> Player -> (DeterDice, Player)
moveDeter d (Player s p) = (d', Player (s+p') p')
    where
    (x,d') = roll3 d
    p' = (p-1+x) `mod` 10 + 1

playDeter :: Game -> Game
playDeter g@(Game d p1 p2)
    | isWinnerDeter p1' = Game d' p1' p2
    | isWinnerDeter p2' = Game d'' p1' p2'
    | otherwise = playDeter g'
    where
    (d', p1') = moveDeter d p1
    (d'', p2') = moveDeter d' p2
    g' = Game d'' p1' p2'

solve1 :: [Int] -> DeterDice
solve1 xs = case (isWinnerDeter p1', isWinnerDeter p2') of
    (True, False) -> (d'-1) * score p2'
    (False, True) -> (d'-1) * score p1'
    _             -> error "unexpected game state"
    where
    Game d' p1' p2' = playDeter (Game 1 p1 p2)
    p1 = Player 0 (head xs)
    p2 = Player 0 (last xs)

type QDice = Int
data Turn = Player1 | Player2 deriving (Eq, Show, Ord)
data QGame = QGame Turn Player Player deriving (Show, Ord, Eq)

move :: QDice -> Player -> Player
move d (Player s p) = Player (s+p') p'
    where
    p' = (p-1+d) `mod` 10 + 1

isWinner :: Player -> Bool
isWinner (Player s p) = s >= 21

play :: Map QGame [Integer] -> QGame -> (Map QGame [Integer], [Integer])
play m g@(QGame turn p1 p2)
    | isWinner p1 = (m,[1,0])
    | isWinner p2 = (m,[0,1])
    | otherwise = case M.lookup g m of
        Just res -> (m, res)
        Nothing  -> (M.insert g res m', res)
    where
    (m',res) = foldl collect (m, [0,0]) universes
    universes = case turn of
        Player1 -> map (\(d,ways) -> (QGame Player2 (move d p1) p2, ways)) outcomes
        Player2 -> map (\(d,ways) -> (QGame Player1 p1 (move d p2), ways)) outcomes
    outcomes = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)] -- I was too stupid for the maths so had to look up for hints

    collect (m,acc) (g,w) = let (m',res) = play m g in
        (m',zipWith (+) acc $ map (*w) res)

solve2 :: [Int] -> Integer
solve2 xs = maximum . snd . play M.empty $ QGame Player1 p1 p2
    where
    p1 = Player 0 (head xs)
    p2 = Player 0 (last xs)

main :: IO ()
main = do
    --file <- readFile "test_input.txt"
    file <- readFile "input.txt"
    putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
    putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)
