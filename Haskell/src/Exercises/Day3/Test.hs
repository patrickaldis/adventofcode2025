module Exercises.Day3.Test where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Exercises.Day3.Lib (findMaxJolt, parseFull)

day3 =
    testGroup
        "Day 3"
        [ testGroup
            "Unit Tests"
            [],
          testGroup
            "Problem"
            [testCase "part 1" $ do
                x <- readFile "src/Exercises/Day3/input.txt"
                (@=?) @Int 16973
                    . sum
                    . fmap findMaxJolt 
                    . parseFull
                    $ x]
        ]
