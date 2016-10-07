import           Data.Array
import Control.Applicative

main :: IO ()
main = do
    x <- readLn
    interact $ unlines . fmap proccess . take x . lines

proccess :: String -> String
proccess line = show $ inversions i1 i2
    where
    [i1, i2] = read <$> words line

inversions :: Int -> Int -> Int
inversions len inv = memfoo ! len ! inv
    --where
foo :: Int -> Int -> Int
foo _ 0 = 1
foo 0 _ = 0
foo n k = sum [memfoo ! (n-1) ! (k-i) | i <- [0..n-1], k - i >= 0]

memfoo = listArray (0, 12) $ flip fmap [0..12] $ \i -> listArray (0, 98) $ fmap (foo i) [0..98]

task = " http://www.spoj.com/problems/PERMUT1/ "
