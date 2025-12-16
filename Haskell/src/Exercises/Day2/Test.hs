module Exercises.Day2.Test where

import Exercises.Day2.Lib (hasCycleOfN, hasCycle)
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

day2 =
    testGroup
        "Unit Tests"
        [ testGroup
            "hasCycleOfN"
            [ 
            testCase "detects 2 cycles" $ hasCycleOfN "hihihi" 2 @?= True,
            testCase "detects 3 cycles" $ hasCycleOfN "history" 3 @?= False
            ],
            testGroup
            "hasCycle"
            [ 
            testCase "detects 2 cycles" $ hasCycle "hihihi" @?= True,
            testCase "identifies no cycles" $ hasCycle "history" @?= False
            ]
        ]
