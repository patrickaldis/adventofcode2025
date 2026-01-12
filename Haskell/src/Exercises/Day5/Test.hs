module Exercises.Day5.Test where

import Data.Either (fromRight)
import Exercises.Day5.Lib
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Text.Megaparsec (count, parse)

day5 =
    testGroup
        "Day 5"
        [ testGroup
            "Unit Tests"
            [ testGroup
                "Parser Tests"
                [ testCase "parseIngredientRange" ( Right (IngredientRange (IngredientID 10) (IngredientID 12)) @=? parse parseIngredientRange "" "10-12"
                    )
                , testCase "parseIngredientDB" ( Right (IngredientDB [IngredientRange (IngredientID 10) (IngredientID 12)] [IngredientID 11]) @=? parse parseDB "" "10-12\n\n11\n"
                    )
                , testCase "parseIngredientDB 2" ( Right (IngredientDB [IngredientRange (IngredientID 10) (IngredientID 12),IngredientRange (IngredientID 13) (IngredientID 14)] [IngredientID 11]) @=? parse parseDB "" "10-12\n13-14\n\n11\n"
                    )
                , testCase "parseIngredientDB 3" ( Right (IngredientDB [IngredientRange (IngredientID 10) (IngredientID 12),IngredientRange (IngredientID 13) (IngredientID 14)] [IngredientID 11, IngredientID 13]) @=? parse parseDB "" "10-12\n13-14\n\n11\n13\n"
                    )
                ]
            ]
        , testGroup
            "Problem"
            [ testCase "Problem" $ do
                input <- readFile "src/Exercises/Day5/input.txt"
                let db = fromRight (IngredientDB [] []) $ parse parseDB "" input
                countFresh db @=? 739
            ]
        ]
