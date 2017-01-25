module IntegrationTests where

import           Test.Tasty.Hspec

import qualified IntegrationTests.Initial
import qualified IntegrationTests.Election


integrationSpec :: Spec
integrationSpec = do
  IntegrationTests.Initial.spec
  IntegrationTests.Election.spec
