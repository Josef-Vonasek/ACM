module Main where

import           Data.Array
import           Data.List

task = " http://www.spoj.com/problems/RENT/ "

data Data = Data {
    start   :: Int,
    length' :: Int,
    price   :: Int
} deriving (Eq, Ord, Show)

main :: IO ()
main' = print $ proccess . take 10000 . join3 Data $ [1..]

main = do
    x <- readLn
    interact $ unlines . fmap (show . proccess) . take x . reduce join . lines

reduce :: ([a] -> ([a], b)) -> [a] -> [b]
reduce _ []     = []
reduce fn list  = b : reduce fn list'
    where
    (list', b) = fn list

join3 fn (a1:a2:a3:aa) = fn a1 a2 a3 : join3 fn aa
join3 fn [] = []

join :: [String] -> ([String], [Data])
join (s:ss) = (rest, fmap foo ss1)
    where
    (ss1, rest) = splitAt (read s) ss
    foo line = Data i1 i2 i3
        where
        [i1, i2, i3] = fmap read (words line)

proccess' is = foo ints
    where
    ints = sort is
    foo (Data i1 i2 i3 :dd) = max (foo dd) (foo (dropWhile ((<(i1+i2)) . start) dd) + i3)
    foo [] = 0

proccess is = memfoo ! len
    where
    len = length is
    sorted :: Array Int Data
    sorted = listArray (1, len) (reverse $ sort is)
    foo :: Int -> Int
    foo 0 = 0
    foo i = max (memfoo ! (i-1)) (i3 + memfoo ! binar ((compare (i1+i2)) . start . (!) sorted) (i))
        where
        Data i1 i2 i3 = sorted ! i
    memfoo = listArray (0, len) [foo i4 | i4 <- [0..]]

binar fn i = decWhile fn (div i 2) step (fn $ div i 2)
    where
    step = if i>4 then div i 4 else 1
decWhile _ 0  _ _ = 0
decWhile fn i 1 ord2
    | ord == ord2 || (ord == EQ && ord2 == LT) = decWhile fn next 1 ord
    | ord2 == GT  = i
    | otherwise   = i-1
    where
    ord = fn i
    next = if ord == GT then (i-1) else (i+1)
decWhile fn i add _ = decWhile fn next (div add 2) (fn $ next)
    where
    next = if fn i == GT then i - add else i + add

ar = listArray (1, 100) [100,99..]
d x = take x . join3 Data $ [1..]

