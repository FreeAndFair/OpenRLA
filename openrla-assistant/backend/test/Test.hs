module Main (main) where

import           Data.Maybe (fromJust, isJust)
import           Test.Tasty
import           Test.Tasty.Hspec (testSpec)
import           Test.Tasty.HUnit

import           IntegrationTests
import           TestSupport


main :: IO ()
main = do
  integrationTests <- testSpec "Integration tests" integrationSpec
  let tests = testGroup "Tests" [unitTests, integrationTests]
  defaultMain tests

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Trivial 1" $ True @?= True
  , testCase "Trivial 2" $ False @?= False
  ]
