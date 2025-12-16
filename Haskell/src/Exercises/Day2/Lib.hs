module Exercises.Day2.Lib where
import Text.Megaparsec (Parsec, sepBy1, many)
import Data.Void (Void)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

hasCycle :: String -> Bool
hasCycle str = any (hasCycleOfN str) [1..(length str `div` 2)]

hasCycleOfN :: String -> Int -> Bool
hasCycleOfN str n = and $ zipWith (==) remaining repeated
    where
        remaining = take n str
        repeated = cycle . drop n $ str

--- PARSER
type Parser = Parsec Void String

parserIdRange :: Parser (Integer, Integer)
parserIdRange = do
    a <- decimal
    _ <- char '-'
    b <- decimal
    pure (a, b)

parseIdLines :: Parser [(Integer, Integer)]
parseIdLines = sepBy1 parserIdRange (char ',')
