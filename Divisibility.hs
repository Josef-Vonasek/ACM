module Main where

main :: IO ()
main = do
    x <- readLn
    interact $ unlines . fmap proccess . take x . lines

proccess :: String -> String
proccess line = unwords . fmap show $ divisable i1 i2 i3
    where
    [i1, i2, i3] = fmap read $ words line

divisable :: Int -> Int -> Int -> [Int]
divisable i1 i2 i3 = filter foo [1..i1]
    where
    foo i = rem i i2 == 0 && rem i i3 /= 0

task = " http://www.spoj.com/problems/SMPDIV/ "
