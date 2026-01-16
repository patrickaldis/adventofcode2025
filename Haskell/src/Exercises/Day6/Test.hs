module Exercises.Day6.Test where

import Data.Either (fromRight)
import Exercises.Day6.Lib
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Text.Megaparsec (count, parse)

day6 =
    testGroup
        "Day 6"
        [ testGroup
            "Unit Tests"
            [ 
            -- testGroup
            --     "Parser Tests"
            --     [ testCase "parseIngredientRange" ( Right (IngredientRange (IngredientID 10) (IngredientID 12)) @=? parse parseIngredientRange "" "10-12"
            --         )
            --     , testCase "parseIngredientDB" ( Right (IngredientDB [IngredientRange (IngredientID 10) (IngredientID 12)] [IngredientID 11]) @=? parse parseDB "" "10-12\n\n11\n"
            --         )
            --     , testCase "parseIngredientDB 2" ( Right (IngredientDB [IngredientRange (IngredientID 10) (IngredientID 12),IngredientRange (IngredientID 13) (IngredientID 14)] [IngredientID 11]) @=? parse parseDB "" "10-12\n13-14\n\n11\n"
            --         )
            --     , testCase "parseIngredientDB 3" ( Right (IngredientDB [IngredientRange (IngredientID 10) (IngredientID 12),IngredientRange (IngredientID 13) (IngredientID 14)] [IngredientID 11, IngredientID 13]) @=? parse parseDB "" "10-12\n13-14\n\n11\n13\n"
            --         )
            --     ]
            ]
        , testGroup
            "Problem"
            [ 
            testCase "Part a" $ do
                input <- readFile "src/Exercises/Day6/input.txt"
                let calcs = fromRight [] $ parse parseCalculations "" input
                grandTotal calcs @=? 5335495999141,
            testCase "Part b" $ do
                input <- readFile "src/Exercises/Day6/input.txt"
                let calcs = fromRight [] $ parse parseCalculationsCephalopod "" input
                grandTotal calcs @=? 5335495999141
            ]
        ]
