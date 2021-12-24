import           Data.List.Split (splitOn)
import           Data.Maybe      (mapMaybe)
import           Debug.Trace     (traceShow)

data ALU = ALU {w :: Int, x :: Int, y :: Int, z :: Int} deriving Show
data Var = W | X | Y | Z deriving Show
data Opr = Num Int | Var Var deriving Show
data Ins = Inp Opr | Add Opr Opr | Mul Opr Opr | Div Opr Opr | Mod Opr Opr | Eql Opr Opr deriving Show

readOpr :: String -> Opr
readOpr "w" = Var W
readOpr "x" = Var X
readOpr "y" = Var Y
readOpr "z" = Var Z
readOpr x   = Num (read x)

readIns :: String -> Ins
readIns x = case splitOn " " x of
    ("inp":a:_)   -> Inp (readOpr a)
    ("mul":a:b:_) -> Mul (readOpr a) (readOpr b)
    ("add":a:b:_) -> Add (readOpr a) (readOpr b)
    ("div":a:b:_) -> Div (readOpr a) (readOpr b)
    ("mod":a:b:_) -> Mod (readOpr a) (readOpr b)
    ("eql":a:b:_) -> Eql (readOpr a) (readOpr b)
    a             -> error $ "Unknown instruction `"++show a++"`"

prepare :: String -> [Ins]
prepare = map readIns . lines

store :: Int -> Var -> ALU -> ALU
store n a (ALU w x y z) = case a of
    W -> ALU n x y z
    X -> ALU w n y z
    Y -> ALU w x n z
    Z -> ALU w x y n

load :: Opr -> ALU -> Int
load (Num n) _ = n
load (Var v) (ALU w x y z) = case v of
    W -> w
    X -> x
    Y -> y
    Z -> z

clock :: ALU -> Int -> Ins -> ALU
clock alu@(ALU w x y z) n i = case i of
    Inp (Var d)     -> store n d alu
    Mul a@(Var d) b -> store (load a alu * load b alu) d alu
    Add a@(Var d) b -> store (load a alu + load b alu) d alu
    Div a@(Var d) b -> store (load a alu `div` load b alu) d alu
    Mod a@(Var d) b -> store (load a alu `mod` load b alu) d alu
    Eql a@(Var d) b -> store (if load a alu == load b alu then 1 else 0) d alu
    _               -> error "Errorous instruction tried to be executed"

run :: ALU -> [Int] -> [Ins] -> ALU
run alu _ [] = alu
run alu inp (i:ins) = case i of
    (Inp _) -> if null inp
        then error "Ran out of input to read from"
        else run alu' ns ins
    _ -> run alu' inp ins
    where
    n:ns = inp
    alu' = clock alu n i

tryInp :: ALU -> [[Int]] -> [Ins] -> [Int]
tryInp _ [] _ = error "Didn't find any number matching the description"
tryInp alu (inp:inps) ins = let (ALU w x y z) = traceShow inp $ run alu inp ins in
    if z == 0
    then inp
    else tryInp alu inps ins

solve1' :: [Ins] -> ALU
solve1' = run alu (digits 79997391969649)
    --tryInp alu inps ins
    where
    alu = ALU 0 0 0 0
    xs = [99999999999999,99999999999998..11111111111111]
    inps = filter (notElem 0) $ map digits xs

digits :: Integral a => a -> [a]
digits n = digits' [] n
    where
    digits' acc 0 = acc
    digits' acc n = digits' (n `mod` 10:acc) (n `div` 10)

fromDigits :: Integral a => [a] -> a
fromDigits [] = 0
fromDigits xs = last xs+10*fromDigits (init xs)

test :: Int -> [(Int,Int,DigitType)] -> [Int] -> Maybe [Int]
test z input ds = test' z input ds []
    where
    test' 0 [] _ d = Just d
    test' z ys gs d = case t of
        Type1 -> test' (z*q + r) xs ws (d++[w])
            where
            p = fromEnum $ (z `mod` 26) + n /= w
            q = 25*p+1
            r = (w+m)*p
        Type2 -> if w' <= 9 && w' >= 1
                then test' ((z `div` 26) * q + r) xs (w:ws) (d++[w'])
                else {-trace (show (fromDigits d)++replicate (14-length d) '?')-} Nothing
            where
            w' = z `mod` 26 + n
            p = fromEnum $ (z `mod` 26) + n /= w'
            q = 25*p+1
            r = (w'+m)*p
        where
        ((n,m,t):xs) = ys
        (w:ws) = gs

data DigitType = Type1 | Type2 deriving Show

-- Hard coded input
nmts :: [(Int, Int, DigitType)]
nmts =
  [ ( 14, 8,  Type1),
    ( 13, 8,  Type1),
    ( 13, 3,  Type1),
    ( 12, 10, Type1),
    (-12, 8,  Type2),
    ( 12, 8,  Type1),
    ( -2, 8,  Type2),
    (-11, 5,  Type2),
    ( 13, 9,  Type1),
    ( 14, 3,  Type1),
    (  0, 4,  Type2),
    (-12, 9,  Type2),
    (-13, 2,  Type2),
    ( -6, 7,  Type2)
  ]

solve1 :: Int
solve1 = fromDigits . head $ mapMaybe (test 0 nmts) ns
    where
    ns = filter (notElem 0) $ map digits [9999999,9999998..1111111]

solve2 :: Int
solve2 = fromDigits . head $ mapMaybe (test 0 nmts) ns
    where
    ns = filter (notElem 0) $ map digits [1111111..9999999]

main :: IO ()
main = do
    --file <- readFile "input.txt"
    putStrLn $ "Part 1 :" ++ show solve1
    putStrLn $ "Part 2 :" ++ show solve2
