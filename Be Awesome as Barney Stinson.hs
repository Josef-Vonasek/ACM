task = "http://www.spoj.com/problems/BEHAPPY/"

main = interact $ unlines . fmap show .  proccess . fmap (fmap read . words)  . lines

proccess ([0, 0]:_)  = []
proccess ([a, b]:ii) = solve gifts remains n : proccess rest
    where
    (ii1, rest) = splitAt a ii
    gifts   = flip fmap ii1 $ \[i1, i2] -> i2 - i1
    remains = scanr1 (+) gifts
    n       = foldl (\i1 (i2:_) -> i1 - i2) b ii1

solve _         _           0 = 1
solve []        []          _ = 0
solve (g:gifts) (r:remains) n
    | n > r = 0
    | otherwise = sum $ fmap foo [0..g]
    where
    foo i = solve gifts remains (n-i)

