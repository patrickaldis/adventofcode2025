
module Exercises.Day3.Lib where

import Data.Char (digitToInt)
import Data.Either (fromRight)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parse)
import Text.Megaparsec.Char (digitChar, newline)

findMaxJolt :: [Int] -> Int
findMaxJolt =
    (\(x, xs) -> (10 * x) + maximum xs)
        . maximumBy (compare `on` fst)
        . init
        . reverse
        . headAndTails

-- [1, 2, 3, 4,] -> [(1, [2,3, 4]), (2, [3, 4])]

headAndTails :: [a] -> [(a, [a])]
headAndTails [x, y] = [(x, [y])]
headAndTails (n : ns) = (n, ns) : headAndTails ns
headAndTails _ = []

-- PARSER
type Parser = Parsec Void String

parseBlock :: Parser [Int]
parseBlock = many digitChar >>= \cs -> pure . fmap digitToInt $ cs

parseBlocks :: Parser [[Int]]
parseBlocks = many (parseBlock >>= \x -> newline >> pure x)

parseFull :: String -> [[Int]]
parseFull str = fromRight [[]] $ parse parseBlocks "" str
