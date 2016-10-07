import Control.Applicative
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as Tio

main = do
    x <- readLn :: IO Int
    Tio.interact $ T.unlines . fmap proccess . take x . T.lines

proccess line = closestPalyndrome' $ line

closestPalyndrome' str
    | F == (any' greater $ T.zip (T.drop (hlen + r) str) (T.reverse $ T.take hlen str)) =
        let
            half = T.take (hlen + r) str
        in  half `T.append` (T.drop r $ T.reverse half)
    | otherwise =
        let
            num = T.foldr inc (T.empty, True) $ T.take (hlen + r) str
        in  case num of
            (half, True) -> '1' `T.cons` (T.drop (1+r) $ T.replicate 2 half) `T.append` T.singleton '1'
            (half, False)-> half `T.append` (T.drop r $ T.reverse half)

    where
    len   = T.length str
    hlen  = div len 2
    r     = rem len 2
    greater (c1, c2) | c1 > c2 = T | c1 < c2 = F | otherwise = N
    inc  c (cc, False) = ( c  `T.cons` cc, False)
    inc '9' (cc, True) = ('0' `T.cons` cc, True)
    inc  c (cc, True)  = ((toEnum . (+1) . fromEnum) c `T.cons` cc, False)

data Bool' = T | F | N deriving (Eq)

any' fn (x:xx) = case fn x of
    T -> T
    F -> F
    N -> any' fn xx
any' _ []      = N

task = " http://www.spoj.com/problems/PALIN/ "
