task = "http://www.spoj.com/problems/FCTRL/"

main = do
    x <- readLn
    interact $ unlines . fmap procces . take x . lines

procces :: String -> String
procces line = show . fnZ . read $ line

fnZ :: Integer -> Integer
fnZ fact = fn pows5
    where
    pows2 = 2 : map (*2) pows2
    pows5 :: [Integer]
    pows5 = 5 : map (*5) pows5
    fn list = foldl (\a b -> (a + div fact b)) 0 $ takeWhile (<=fact) list

