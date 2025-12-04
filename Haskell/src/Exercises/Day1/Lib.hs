{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}

module Exercises.Day1.Lib where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

data Move = MoveRight Integer | MoveLeft Integer deriving (Eq, Show)

countWith :: (Num c) => (a -> Bool) -> [a] -> c
countWith f = fromIntegral . length . filter f

countExactZeros :: [Integer] -> Integer
countExactZeros = countWith (== 0)

cumSumMod :: [Move] -> [Integer]
cumSumMod = fmap (`mod` 100) . cumSum

cumSum :: [Move] -> [Integer]
cumSum = scanl addMove 50
    where
        addMove :: Integer -> Move -> Integer
        addMove acc (MoveRight n) = acc - n
        addMove acc (MoveLeft n) = acc + n

getPassword :: [Move] -> Integer
getPassword = countExactZeros . cumSumMod

--- PARSER
type Parser = Parsec Void String

parseLine :: Parser Move
parseLine =
    choice
        [ char 'L' >> MoveLeft <$> decimal
        , char 'R' >> MoveRight <$> decimal
        ]

parseLines :: Parser [Move]
parseLines = parseLine `sepEndBy` eol

toSequence :: String -> [Move]
toSequence s = case parse parseLines "" s of
    Left err -> []
    Right seq -> seq

-- PART 2
countZeros :: Num a => [Integer] -> a
countZeros s = sum $ flip fmap (zip s $ tail s) \(a, b) ->
    let
        endZeros = if b `mod` 100 == 0 then 1 else 0
        movingZeros = abs . fromInteger $ (a `div` 100) - (b `div` 100)
        doubleCount = if b `mod` 100 == 0 && a < b then 1 else 0
    in endZeros + movingZeros - doubleCount

getPassword' :: [Move] -> Integer
getPassword' = countZeros . cumSum
