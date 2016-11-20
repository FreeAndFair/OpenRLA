{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit

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
