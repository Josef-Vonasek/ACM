{-# LANGUAGE BangPatterns #-}
module Main where
import           Data.Array

task = "http://www.spoj.com/problems/TRT/"

main :: IO ()
main' = print $ most' [1..2000]
main = do
    x <- readLn
    interact $ flip (++) n . proccess . take x . lines

proccess :: [String] -> String
proccess ss = show . most . fmap read $ ss

--most :: [Integer] -> Integer
most [] = 0
most is = fst . foldl bar (last begin, begin) $ [1..len]
    where
    len :: Integer
    len = fromIntegral $ length is
    prices = listArray (0, len-1) is
    begin = 0 : zipWith3 (\i1 i2 i3 -> i1 + i2 * i3) begin (reverse is) [1..]
    foo _ _ _ [_] = []
    foo ix1 ix2 i1 (i2:ii) = res : foo ix1 (ix2+1) res ii
        where
        age = ix1 + ix2
    bar (best, [x]) i    = (max best x, [])
    bar (best, (x:xx)) i = (max best best', list')
        where
        start = x + prices ! (i-1) * i

most' :: [Int] -> Int
most' is = maximum $ fmap (\i -> memfoo ! (i, (len - i))) [0..len]
    where
    prices = listArray (0, len) is
    len = length is
    foo 0 0 = 0
    foo i 0 = memfoo ! (i-1, 0)   + prices ! (i-1)   * i
    foo 0 i = memfoo ! (0, i-1)   + prices ! (len-i) * i
    foo i1 i2
        | age > len = -1
        | otherwise = max
            (memfoo ! (i1-1, i2) +     prices ! (i1-1)    * age)
            (memfoo ! (i1, i2-1) +     prices ! (len-i2)  * age)
        where
        age = i1 + i2
    memfoo :: Array (Int, Int) Int
    memfoo = listArray ((0, 0), (len, len)) [foo i1 i2 | i1 <- [0..len], i2 <- [0..len]]

