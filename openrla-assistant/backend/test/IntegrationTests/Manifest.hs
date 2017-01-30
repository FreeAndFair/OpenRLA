module IntegrationTests.Manifest where

import qualified Data.Aeson as A
import           Data.Aeson (Value(..))
import           Data.HashMap.Lazy ((!))
import           Data.Maybe (fromJust)
import qualified System.Directory as Dir
import           System.FilePath ((</>))

import           Network.Wai.Test (SResponse(..), simpleBody)
import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import           JsonTestSupport
import           TestSupport


candidateManifestPath :: FilePath
candidateManifestPath = "dominion" </> "CandidateManifest.json"

electionPostBody = [json|{
    title: "POTUS 2016",
    date: "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)"
}|]

testFileIO :: IO (FilePath -> FilePath)
testFileIO = do
  curDir <- Dir.getCurrentDirectory
  let testFile relPath = curDir </> "test" </> "data" </> relPath
  return testFile

spec :: Spec
spec = do
  around withApp $ context "Candidate manifests" $ do
    let mkPostBody = \testFile -> [json|{
      electionId: 1,
      vendor: "dominion",
      type: "candidate",
      filePath: #{testFile candidateManifestPath}
    }|]

    it "processes a valid candidate manifest" $ do
      testFile <- liftIO testFileIO
      let postBody = mkPostBody testFile

      postJson "/election" electionPostBody

      let resp = postJson "/manifest" postBody

      resp `shouldRespondWith` 200

      SResponse { simpleBody } <- resp
      liftIO $ do
        let body = fromJust $ A.decode simpleBody :: Value
            bodyData = case body of
              Object o -> o ! "data"
        (jsonLength bodyData) `shouldBe` (Just 20)
