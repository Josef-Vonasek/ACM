task = "http://www.spoj.com/problems/LASTDIG/"

main = do
    x <- readLn
    interact $ unlines . fmap (show . unwrap lastDigit) . fmap (fmap read . words) . take x . lines
    where
    unwrap fn [a, b] = fn a b

lastDigit :: Int -> Int -> Int
lastDigit _    0    = 1
lastDigit base pow  = foo base' pow
    where
    base' = rem base 10
    mem 2 = jc [2,4,8,6]
    mem 3 = jc [3,9,7,1]
    mem 4 = jc [4,6]
    mem 7 = jc [7,9,3,1]
    mem 8 = jc [8,4,2,6]
    mem 9 = jc [9,1]
    mem _ = Nothing
    jc = Just
    foo a b = case mem a of
        Nothing -> a
        Just ii -> ii !! (rem (b-1) (length ii))

