import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, satisfy, spaces, space, letter)
import Text.Parsec.Combinator (many1, choice, chainl1, sepBy)
import Text.Parsec.Token (symbol)
import Control.Applicative
import Control.Monad (void)
import Data.Char (isLetter, isDigit)

operators = "+-*/^"

data Expr = Expr Char Expr Expr | Var Char

instance Show Expr where
    show (Expr op e1 e2) = (show e1) ++ (show e2) ++ [op]
    show (Var var)       = [var]

ss :: Parser ()
ss = spaces

var = Var <$> letter

parens = do
    char '('
    e <- expr
    char ')'
    return e

term = (>>) ss $ var <|> parens

expr = chainl1 term op
    where
    op = spaces >> oneOf operators >>= (\a -> return (Expr a))

main = do
    x <- readLn
    interact $ unlines . fmap proccess . take x . lines
    return 0

proccess line = show rpn where Right rpn = parse expr "" line

task = " http://www.spoj.com/problems/ONP/ "
