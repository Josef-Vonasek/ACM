import Data.Array

task = "http://www.spoj.com/problems/UOFTAE/"

main = do
    x <- readLn
    interact $ unlines . fmap show .  proccess x . fmap (fmap read . words)  . lines

proccess 0 _  = []
proccess x ([a, b]:ii) = foo gifts remains n : proccess (x-1) rest
    where
    (ii1, rest) = splitAt a ii
    gifts   = flip fmap ii1 $ \[i1, i2] -> i2 - i1
    remains = scanr1 (+) gifts
    n       = foldl (\i1 (i2:_) -> i1 - i2) b ii1

foo :: [Int] -> [Int] -> Int -> Int
foo a b c
    | c < 0     = 0
    | otherwise = memsolve ! (0, c)
    where
    len = length a
    gifts = listArray (0, len-1) a
    remains = listArray (0, length b -1) b
    solve _  0 = 1
    --solve []        []          _ = 0
    solve i1 i2
        | i1 == len = 0
        | i2 > remains ! i1 = 0
        | otherwise = sum $ fmap foo' [0..min (gifts ! i1) i2]
        where
        foo' i = memsolve ! (i1+1, i2 - i)

    memsolve = listArray ((0, 0), (len, c)) [solve i1 i2 | i1 <- [0..len], i2 <- [0..c]]
