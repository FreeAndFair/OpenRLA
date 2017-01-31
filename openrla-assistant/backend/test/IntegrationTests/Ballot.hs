module IntegrationTests.Ballot where

import           Data.Aeson (Value(..))
import qualified System.Directory as Dir
import           System.FilePath ((</>))

import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import           JsonTestSupport
import           TestSupport


date :: String
date = "Tue Jan 01 2016 12:34:56 GMT-0000 (UTC)"

electionPostBody :: Value
electionPostBody = [json|{
  title: "The Election",
  date: #{date}
}|]

testFileIO :: IO (FilePath -> FilePath)
testFileIO = do
  curDir <- Dir.getCurrentDirectory
  let testFile relPath = curDir </> "test" </> "data" </> "dominion" </> relPath
  return testFile

spec :: Spec
spec = do
  around withApp $ context "Ballot" $ do
    it "should create ballots for an election" $ do
      testFile <- liftIO testFileIO
      let ballotPath i = testFile $ "test-ballots" </> ("ballot-" ++ show i)

      postJson "/election" electionPostBody

      let ballotSrcPaths = map ballotPath [1 .. 100]
          ballotPostBody = [json|{
            electionId: 1,
            filePaths: #{ballotSrcPaths}
          }|]

      createResp <- postJson "/ballot" ballotPostBody

      return createResp `shouldRespondWith` 200

      let createBody = decodeBody createResp
      liftIO $ jsonLength createBody `shouldBe` 100

      byIdResp <- get "/ballot/10"

      return byIdResp `shouldRespondWith` 200

      let byIdBody = decodeBody byIdResp
      liftIO $ (byIdBody .! "id") `shouldBe` Number 10

      get "/ballot/101" `shouldRespondWith` 404

      indexResp <- get "/election/1/ballot"

      return indexResp `shouldRespondWith` 200

      let indexBody = decodeBody indexResp
      liftIO $ jsonLength indexBody `shouldBe` 100
