module Exercises.Day4.Test where

import Data.Massiv.Array
import Data.Maybe (fromJust)
import Exercises.Day3.Lib (findMaxJolt, parseFull)
import Exercises.Day4.Lib (getLocations)
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

day4 =
    testGroup
        "Day 4"
        [ testGroup
            "Unit Tests"
            [ testCase
                "getLocations"
                (13
                    @=? getLocations
                        ( fromJust
                            ( fromListsM
                                Seq
                                [ [True, True, True, False, True, False, True, False, True, True]
                                , [True, True, True, True, True, False, True, False, True, True]
                                , [True, False, True, True, True, True, False, False, True, False]
                                , [True, True, False, True, True, True, True, False, True, True]
                                , [False, True, True, True, True, True, True, True, False, True]
                                , [False, True, False, True, False, True, False, True, True, True]
                                , [True, False, True, True, True, False, True, True, True, True]
                                , [False, True, True, True, True, True, True, True, True, False]
                                , [True, False, True, False, True, True, True, False, True, False]
                                ] ::
                                Maybe (Array B Ix2 Bool)
                            )
                        )
                )
            ]
        , testGroup
            "Problem"
            []
        ]
