module Exercises.Day6.Lib where

import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, space, hspace, eol, digitChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.List (transpose)
import Data.Maybe (catMaybes)

data Calculation = Calculation Operation [Int] deriving (Show, Eq)
data Operation = Multiply | Add deriving (Show, Eq)

grandTotal :: [Calculation] -> Int
grandTotal = sum . fmap calculate

calculate :: Calculation -> Int
calculate (Calculation Multiply ns) = product ns
calculate (Calculation Add ns) = sum ns


type Parser = Parsec Void String

parseCalculationRow :: Parser [Int]
parseCalculationRow = many (decimal <* hspace) <* eol

parseOperatorRow :: Parser [Operation]
parseOperatorRow =
    many
        ( choice
            [ char '+' $> Add
            , char '*' $> Multiply
            ]
            <* space
        )

parseCalculations :: Parser [Calculation]
parseCalculations = do
    (numRows, opRow) <- manyTill_ parseCalculationRow parseOperatorRow
    pure $ zipWith Calculation opRow (transpose numRows)

parseCalculationRowCephalopod :: Parser [Maybe Char]
parseCalculationRowCephalopod = many (choice [
        Just <$> digitChar,
        char ' ' $> Nothing
    ]) <* eol

cephalise :: [[Maybe Char]] -> [[Int]]
cephalise = _ . fmap catMaybes . transpose

parseCalculationsCephalopod :: Parser [Calculation]
parseCalculationsCephalopod = do
    numRows <- count 4 parseCalculationRowCephalopod
    opRow <- parseOperatorRow
    pure $ zipWith Calculation opRow (cephalise numRows)

