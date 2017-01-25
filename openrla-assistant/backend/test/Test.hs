module Main (main) where

import           Test.Tasty
import           Test.Tasty.Hspec (testSpec)

import           IntegrationTests


main :: IO ()
main = do
  integrationTests <- testSpec "Integration tests" integrationSpec
  let tests = testGroup "Tests" $ [unitTests, integrationTests]
  defaultMain tests

unitTests :: TestTree
unitTests = testGroup "Unit tests" []
