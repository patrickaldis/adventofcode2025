module Main where

import Test.Tasty
import Exercises.Day1.Test
import Exercises.Day2.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    day1
