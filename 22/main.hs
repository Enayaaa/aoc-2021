import           Data.List.Split (splitOn)
import qualified Data.Set        as S

data State = On | Off deriving Show
type Range = (Int,Int)
data Region = Region {state :: State, xrange :: (Int,Int), yrange :: (Int,Int), zrange :: (Int,Int)}

showRange :: Range -> String
showRange (i,f) = show i++".."++show f

instance Show Region where
    show (Region st xr yr zr) =
        show st++" x="++showRange xr++" y="++showRange yr++" z="++showRange zr

parseState :: String -> State
parseState "on"  = On
parseState "off" = Off
parseState x     = error $ "parsing `"++x++"` as State failed."

parseRegion :: String -> Region
parseRegion l = Region st xr yr zr
    where
    (pre:rest:_) = splitOn " " l
    st = parseState pre
    (xr:yr:zr:_) = map ((\(a:b:_)->(a,b)) . map read . splitOn ".." . drop 2) $ splitOn "," rest

prepare :: String -> [Region]
prepare = map parseRegion . lines

filter1 :: Region -> Bool
filter1 (Region st (xi,xf) (yi,yf) (zi,zf)) =
    xi >= -50 && xf <= 50 &&
    yi >= -50 && yf <= 50 &&
    zf >= -50 && zf <= 50

isOn :: Region -> Bool
isOn (Region On _ _ _)  = True
isOn (Region Off _ _ _) = False

regionToPoints :: Region -> [(Int, Int, Int)]
regionToPoints (Region st (xi,xf) (yi,yf) (zi,zf)) =
    [(x,y,z) | x <- [xi..xf], y <- [yi..yf], z <- [zi..zf]]

isValidRegion :: Region -> Bool
isValidRegion r = uncurry (<=) (xrange r) && uncurry (<=) (yrange r) && uncurry (<=) (zrange r)

overlapsOnAxis :: Range -> Range -> Bool
overlapsOnAxis (x1i,x1f) (x2i,x2f) = x2i <= x1f && x1i <= x2f

getAxisOverlap :: Range -> Range -> Range
getAxisOverlap (x1i,x1f) (x2i,x2f) = (max x1i x2i, min x1f x2f)

intersection :: Region -> Region -> Maybe Region
intersection (Region st1 xr1 yr1 zr1) (Region st2 xr2 yr2 zr2)
    | overlapsOnAxis xr1 xr2 && overlapsOnAxis yr1 yr2 && overlapsOnAxis zr1 zr2
        = Just $ Region On xr yr zr
    | otherwise = Nothing
    where
    xr = getAxisOverlap xr1 xr2
    yr = getAxisOverlap yr1 yr2
    zr = getAxisOverlap zr1 zr2

regionSize :: Region -> Int
regionSize (Region st (xi,xf) (yi,yf) (zi,zf)) = (xf-xi+1)*(yf-yi+1)*(zf-zi+1)

carveOut :: Region -> Region -> [Region]
carveOut a b = case intersection a b of
  Nothing -> [a]
  Just o -> filter isValidRegion potential
    where
    st = state a
    (axi,axf) = xrange a
    (ayi,ayf) = yrange a
    (azi,azf) = zrange a
    (oxi,oxf) = xrange o
    (oyi,oyf) = yrange o
    (ozi,ozf) = zrange o
    potential = [
        Region st (xrange o) (ayi, oyi-1) (zrange o),
        Region st (xrange o) (oyf+1, ayf) (zrange o),
        Region st (xrange a) (yrange a) (azi, ozi-1),
        Region st (xrange a) (yrange a) (ozf+1, azf),
        Region st (axi, oxi-1) (yrange a) (zrange o),
        Region st (oxf+1, axf) (yrange a) (zrange o)
        ]

solve1' :: [Region] -> Int
solve1' = helper S.empty . filter filter1
    where
    helper s [] = S.size s
    helper s (x:xs) = case state x of
        On  -> helper (S.union s (S.fromList $ regionToPoints x)) xs
        Off -> helper (S.difference s (S.fromList $ regionToPoints x)) xs

solve :: [Region] -> Int
solve xs = sum $ map regionSize $ helper [] xs
    where
    helper acc [] = acc
    helper acc (x:xs)
        | isOn x = helper (x:concatMap (`carveOut` x) acc) xs
        | otherwise = helper (concatMap (`carveOut` x) acc) xs

solve1 :: [Region] -> Int
solve1 = solve . filter filter1

solve2 :: [Region] -> Int
solve2 = solve

main :: IO ()
main = do
    --file <- readFile "test_input.txt"
    --file <- readFile "sample.txt"
    --file <- readFile "sample2.txt"
    file <- readFile "input.txt"
    putStrLn $ "Part 1 :" ++ (show . solve1 . prepare $ file)
    putStrLn $ "Part 2 :" ++ (show . solve2 . prepare $ file)
