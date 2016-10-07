module Main where

main :: IO ()
main = interact $ unlines . fmap proccess . join . takeWhile (/= "0") . lines
    where
    join (s1:s2:ss) = (s1, s2) : join ss
    join _ = []

proccess :: (String, String) -> String
proccess (s1, s2)
    | possible is = "yes"
    | otherwise   = "no"
    where
    is = fmap read $ words s2

possible :: [Int] -> Bool
possible = foo 1 []
    where
    foo next stack (o:order)
        | next == o                                     = foo (next+1) stack order
        | not (null stack) && head stack == next      = foo (next+1) (tail stack) (o:order)
        | otherwise                                     = foo next (o:stack) order
    foo next (s:stack) []
        | next == s = foo (next+1) stack []
        | otherwise = False
    foo _ [] [] = True

task = " http://www.spoj.com/problems/STPAR/ "
