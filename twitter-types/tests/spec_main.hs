module Main where

import Test.Tasty
import qualified TypesTest
import qualified PropFromToJSONTest

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ testGroup "Unit Test" [TypesTest.tests]
        , testGroup "Property Test" [PropFromToJSONTest.tests]
        ]

main :: IO ()
main = defaultMain tests
