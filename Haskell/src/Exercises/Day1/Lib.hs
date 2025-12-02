module Exercises.Day1.Lib where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

type Sequence = [Integer]

getPassword :: Sequence -> Integer
getPassword = countZeros . cumSum

countZeros :: Sequence -> Integer
countZeros = fromIntegral . length . filter (== 0)

cumSum :: Sequence -> Sequence
cumSum = scanl (\acc elem -> (`mod` 100) $ acc + elem) 50

--- PARSER

type Parser = Parsec Void String

parseLine :: Parser Integer
parseLine =
    choice
        [ char 'L' >> decimal
        , char 'R' >> negate <$> decimal
        ]

parseLines :: Parser Sequence
parseLines = parseLine `sepEndBy` eol

toSequence :: String -> Sequence
toSequence s = case parse parseLines "" s of
    Left err -> []
    Right seq -> seq
