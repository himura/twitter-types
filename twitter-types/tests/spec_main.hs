module Main where

import Test.Tasty
import qualified TypesTest

tests :: TestTree
tests = testGroup "twitter tests" [TypesTest.tests]

main :: IO ()
main = defaultMain tests
