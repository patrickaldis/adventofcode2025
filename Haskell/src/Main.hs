module Main where

import Test.Tasty
import Exercises.Day1.Test
import Exercises.Day2.Test
import Exercises.Day3.Test (day3)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Tests"
    [
    day1,
    day2,
    day3
    ]
