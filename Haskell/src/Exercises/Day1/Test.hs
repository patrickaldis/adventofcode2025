module Exercises.Day1.Test where

import Debug.Trace
import Exercises.Day1.Lib
import Test.Tasty
import Test.Tasty.HUnit

day1 :: TestTree
day1 =
    testGroup
        "Day 1"
        [ testGroup
            "Unit Tests"
            [ testCase "cumSum" $ cumSum [1, 2, -3] @?= [50, 51, 53, 50]
            , testCase "countZeros" $ countZeros [1, 2, 0, 0, 1, 2] @?= 2
            , testCase "toSequence" $ toSequence "R1\nL2\n" @?= [-1, 2]
            ]
        , testCase "problem" $ do
            x <- readFile "src/Exercises/Day1/input.txt"
            (@=?) @Integer 1052
                . countZeros
                . traceShowId
                . cumSum
                . toSequence
                $ x
        ]
