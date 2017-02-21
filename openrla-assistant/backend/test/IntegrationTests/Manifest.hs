module IntegrationTests.Manifest where

import           Data.Aeson (Value(..))
import qualified System.Directory as Dir
import           System.FilePath ((</>))

import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import qualified IntegrationTests.Expected as Expected
import           JsonTestSupport
import           TestSupport


electionPostBody :: Value
electionPostBody = [json|{
    title: "POTUS 2016",
    date: "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)"
}|]

testFileIO :: IO (FilePath -> FilePath)
testFileIO = do
  curDir <- Dir.getCurrentDirectory
  let testFile relPath = curDir </> "test" </> "data" </> relPath
  return testFile

manifestPostBodyIO :: IO (String -> FilePath -> Value)
manifestPostBodyIO = do
  testFile <- testFileIO
  let manifestPostBody mType path = [json|{
    electionId: 1,
    vendor: "dominion",
    type: #{mType},
    filePath: #{testFile path}
  }|]
  return manifestPostBody

spec :: Spec
spec = do
  around withApp $ context "Uploading example manifests" $ do
    let candidatePath = "dominion" </> "example" </> "CandidateManifest.json"
        contestPath = "dominion" </> "example" </>"ContestManifest.json"

    it "should process them when uploaded in the correct order" $ do
      manifestPostBody <- liftIO manifestPostBodyIO

      -- When we have an election
      postJson "/election" electionPostBody

      -- We can upload a contest manifest
      let contestPostBody = manifestPostBody "contest" contestPath

      contestResp <- postJson "/manifest" contestPostBody

      liftIO $ do
        let body = decodeBody contestResp
            bodyData = body .! "data"
        jsonLength bodyData `shouldBe` 15

      -- We can upload a candidate manifest
      let candidatePostBody = manifestPostBody "candidate" candidatePath

      candidateResp <- postJson "/manifest" candidatePostBody

      return candidateResp `shouldRespondWith` 200

      liftIO $ do
        let body = decodeBody candidateResp
            bodyData = body .! "data"
        jsonLength bodyData `shouldBe` 20

      -- And its fully-defined contests will be associated with the election
      electionResp <- get "/election/1/contest"

      let body = decodeBody electionResp
      liftIO $ body `shouldBe` Expected.contestsWithCandidates

  around withApp $ context "Uploading synthetic test manifests" $ do
    let relPath = ("dominion" </>)
        ballotPath = relPath "TestBallotManifest.json"
        candidatePath = relPath "TestCandidateManifest.json"
        contestPath = relPath "TestContestManifest.json"

    -- Note: we do this test separately from the above, and with
    -- synthetic data,because we currently need to control the values of
    -- the `ImageMask` keys within the manifest, and ensure that they
    -- describe paths to actual files.
    it "should allow uploading a ballot manifest" $ do
      manifestPostBody <- liftIO manifestPostBodyIO

      -- When we have an election
      postJson "/election" electionPostBody

      -- And we've uploaded a contest manifest
      let contestPostBody = manifestPostBody "contest" contestPath
      contestResp <- postJson "/manifest" contestPostBody
      return contestResp `shouldRespondWith` 200

      -- And also a candidate manifest
      let candidatePostBody = manifestPostBody "candidate" candidatePath
      candidateResp <- postJson "/manifest" candidatePostBody
      return candidateResp `shouldRespondWith` 200

      -- We can upload a ballot manifest
      let ballotPostBody = manifestPostBody "ballot" ballotPath

      postJson "/manifest" ballotPostBody `shouldRespondWith` 200

      -- And fetch the uploaded ballots
      ballotResp <- get "/election/1/ballot"
      return ballotResp `shouldRespondWith` 200

      ballotResp `bodyShouldBe` [json|[
        {
          id: 1,
          srcPath: "./test/data/dominion/test-ballots/ballot-2",
          filePath: ""
        },
        {
          id: 2,
          srcPath: "./test/data/dominion/test-ballots/ballot-3",
          filePath: ""
        },
        {
          id: 3,
          srcPath: "./test/data/dominion/test-ballots/ballot-5",
          filePath: ""
        }
      ]|]
