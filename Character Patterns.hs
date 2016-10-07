main :: IO ()
main = do
    x <- readLn
    interact $ unlines . fmap proccess . take x . lines

proccess :: String -> String
proccess line = chessPattern r c
    where
    (r:c:[]) = fmap read $ words line

chessPattern :: Int -> Int -> String
chessPattern r c = unlines $ take r $ fmap (take c) chessP
    where
    pattern = '*' : '.' : fmap id pattern
    chessP  = pattern : tail pattern : fmap id chessP

task = " http://www.spoj.com/problems/CPTTRN1/ "
