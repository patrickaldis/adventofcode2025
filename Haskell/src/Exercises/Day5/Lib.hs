{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Exercises.Day5.Lib where

import Prelude hiding (id)
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char (char, newline)

newtype IngredientID = IngredientID Int deriving (Eq, Show)

data IngredientRange = IngredientRange IngredientID IngredientID deriving (Eq, Show)

isFresh :: [IngredientRange] -> IngredientID -> Bool
isFresh rules (IngredientID id) =
    any
        (\(IngredientRange (IngredientID start) (IngredientID stop)) -> start <= id && id <= stop)
        rules

countFresh :: IngredientDB -> Int
countFresh (IngredientDB rules ids) = length $ filter (isFresh rules) ids

-- PARSER
type Parser = Parsec Void String

parseIngredientRange :: Parser IngredientRange
parseIngredientRange = do
    start <- parseIngredientID 
    char '-'
    stop <- parseIngredientID
    pure $ IngredientRange start stop

parseIngredientID :: Parser IngredientID
parseIngredientID = IngredientID <$> decimal

data IngredientDB = IngredientDB {
    ingredientRanges :: [IngredientRange],
    ingredients :: [IngredientID]
} deriving (Eq, Show)

parseDB :: Parser IngredientDB
parseDB = do
    ingredientRanges <- many (parseIngredientRange <* newline)
    newline
    ingredients <- many (parseIngredientID <* newline)
    pure $ IngredientDB ingredientRanges ingredients

