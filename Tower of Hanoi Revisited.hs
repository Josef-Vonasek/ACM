module Main where
import           Data.Int

task = " http://www.spoj.com/problems/RANJAN02/ "

main :: IO ()
main = do
    x <- readLn
    interact $ unlines . fmap proccess . take x . lines

proccess :: String -> String
proccess line = show $ hanoi index
    where
    index = read line

hanoi :: Int -> Int64
hanoi x = memfoo !! x
    where
    memfoo = 0 : fmap ((+2).(*3)) memfoo

