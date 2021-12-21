{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.List.Split
import Debug.Trace
import Data.List
import qualified Data.MemoCombinators as Memo
--import Data.MemoCombinators.Class (memoize)


type DeterDice = Int
data Player = Player {score :: Int, pos :: Int} deriving (Show, Ord, Eq)
data Game = Game DeterDice Player Player deriving Show

prepare = map (read @Int . last . splitOn ": ") . lines

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

solve1 xs = case (isWinnerDeter p1', isWinnerDeter p2') of
    (True, False) -> (d'-1) * score p2'
    (False, True) -> (d'-1) * score p1'
    _ -> error "unexpected game state"
    where
    Game d' p1' p2' = playDeter (Game 1 p1 p2)
    p1 = Player 0 (head xs)
    p2 = Player 0 (last xs)

type QDice = Int
--data Turn = Player1 | Player2 deriving (Eq, Show, Ord)
data QGame = QGame Int Player Player deriving (Show, Ord, Eq)

move :: QDice -> Player -> Player
move d (Player s p) = Player (s+p') p'
    where
    p' = (p-1+d) `mod` 10 + 1

isWinner :: Player -> Bool
isWinner (Player s p) = s >= 21

playMemo (QGame turn (Player s1 p1) (Player s2 p2)) =  f [s1, p1, s2, p2]
    where
        f = Memo.list Memo.integral play

--play :: QGame -> [Integer]
play [] = error "what ??"
play (turn:s1:p1:s2:p2:_)
    | s1 >= 21 = [1,0]
    | s2 >= 21 = [0,1]
    | otherwise = map sum . transpose $ map playMemo gs
        --map sum $ map (\(p1', p2') -> play (QGame p1' p2')) ys
    where
    gs = [if turn == 1 then QGame 2 (pl1 (d1+d2+d3)) (Player s2 p2) else QGame 1 (Player s1 p1) (pl2 (d1+d2+d3)) | d1 <- [1..3], d2 <- [1..3], d3 <- [1..3]]
    pl1 d = move d $ Player s1 p1
    pl2 d = move d $ Player s2 p2

solve2 xs = playMemo $ QGame 1 p1 p2
    where
    p1 = Player 0 (head xs)
    p2 = Player 0 (last xs)

main = do
    file <- readFile "test_input.txt"
    --file <- readFile "input.txt"
    putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
    putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)

