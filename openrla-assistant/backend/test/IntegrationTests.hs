module IntegrationTests where

import           Test.Tasty.Hspec

import qualified IntegrationTests.Audit
import qualified IntegrationTests.Ballot
import qualified IntegrationTests.Initial
import qualified IntegrationTests.Election
import qualified IntegrationTests.Manifest


integrationSpec :: Spec
integrationSpec = do
  IntegrationTests.Audit.spec
  IntegrationTests.Ballot.spec
  IntegrationTests.Initial.spec
  IntegrationTests.Election.spec
  IntegrationTests.Manifest.spec
