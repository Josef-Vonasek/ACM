import Control.Monad
import Control.Applicative

primesBetween :: Int -> Int -> [Int]
primesBetween gt ls = filter isprime [gt..ls]
    where
    isprime 1 = False
    isprime 2 = True
    isprime p = all (\d -> rem p d > 0) (takeWhile (<p) (primes))
    primes = primesTo 31622

primesTo ls = 2 : sieve [3,5..ls]
    where
    sieve (x:xx)
        | x*x > ls = x:xx
        | True     = x:sieve [p | p <- xx, rem p x > 0]

nlines fn n = interact $ unlines . fn . take n . lines

main = read <$> getLine >>= nlines (concat . fmap (splitAndCall primesBetween))
    where
    splitAndCall fn line = (show <$> fn a b) ++ [""]
        where
        (a:b:[]) = read <$> words line

task = " http://www.spoj.com/problems/PRIME1/ "
