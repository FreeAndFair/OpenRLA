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

mkManifestPostBody :: Integer -> String -> String -> FilePath -> A.Value
mkManifestPostBody elId vendor manifestType filePath = [json|{
  electionId: #{elId},
  vendor: #{vendor},
  type: #{manifestType},
  filePath: #{filePath}
}|]

testFile :: FilePath -> IO FilePath
testFile relPath = do
  curDir <- Dir.getCurrentDirectory
  let testDataDir = curDir </> "test" </> "data"
  return $ testDataDir </> relPath

spec :: Spec
spec = do
  around withApp $ context "Creating a first election" $ do
    it "processes a valid candidate manifest" $ do
      postBody <- liftIO $ do
        absPath <- testFile candidateManifestPath
        return $ mkManifestPostBody 1 "dominion" "candidate" absPath

      postJson "/election" electionPostBody

      let resp = postJson "/manifest" postBody

      resp `shouldRespondWith` 200

      SResponse { simpleBody } <- resp
      liftIO $ do
        let body = fromJust $ A.decode simpleBody :: Value
            bodyData = case body of
              Object o -> o ! "data"
        (jsonLength bodyData) `shouldBe` (Just 20)
