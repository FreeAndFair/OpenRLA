module IntegrationTests.Manifest where

import qualified System.Directory as Dir

import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import           TestSupport


candidateManifestPath :: FilePath
candidateManifestPath = "test/data/dominion/CandidateManifest.json"

electionPostBody = [matchJson|{
    title: "POTUS 2016",
    date: "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)"
}|]

spec :: Spec
spec = do
  around withApp $ context "Creating a first election" $ do
    it "processes a valid candidate manifest" $ do
      post "/election" electionPostBody `shouldRespondWith` 200
