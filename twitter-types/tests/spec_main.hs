module Main where

import qualified PropFromToJSONTest
import qualified StatusTest
import Test.Tasty
import qualified TypesTest

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ testGroup "Unit Test" [TypesTest.tests, StatusTest.tests]
        , testGroup "Property Test" [PropFromToJSONTest.tests]
        ]

main :: IO ()
main = defaultMain tests
