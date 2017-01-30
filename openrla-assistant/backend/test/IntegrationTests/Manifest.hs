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


candidatePath :: FilePath
candidatePath = "dominion" </> "CandidateManifest.json"

contestPath :: FilePath
contestPath = "dominion" </> "ContestManifest.json"

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
  around withApp $ context "Uploading manifests" $ do
    it "should process them when uploaded in the correct order" $ do
      manifestPostBody <- liftIO manifestPostBodyIO

      -- When we have an election
      postJson "/election" electionPostBody

      -- We can upload a contest manifest
      let contestPostBody = manifestPostBody "contest" contestPath
          contestResp = postJson "/manifest" contestPostBody

      SResponse { simpleBody } <- contestResp
      liftIO $ do
        let body = fromJust $ A.decode simpleBody :: Value
            bodyData = case body of
              Object o -> o ! "data"
        (jsonLength bodyData) `shouldBe` (Just 15)

      -- We can upload a candidate manifest
      let candidatePostBody = manifestPostBody "candidate" candidatePath
          candidateResp = postJson "/manifest" candidatePostBody

      candidateResp `shouldRespondWith` 200

      SResponse { simpleBody } <- candidateResp
      liftIO $ do
        let body = fromJust $ A.decode simpleBody :: Value
            bodyData = case body of
              Object o -> o ! "data"
        (jsonLength bodyData) `shouldBe` (Just 20)
