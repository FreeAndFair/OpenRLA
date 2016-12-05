module Main (main) where

import           Data.Maybe (fromJust, isJust)
import           Test.Tasty
import           Test.Tasty.HUnit

import           TestSupport
-- import           OpenRLA (app)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Trivial 1" $ True @?= True
  , testCase "Trivial 2" $ False @?= False
  ]
