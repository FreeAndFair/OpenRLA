module IntegrationTests.Fixture where

import           Data.Aeson (Value(..))
import qualified System.Directory as Dir
import           System.FilePath ((</>))

import           Test.Hspec.Wai

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

withElection :: WaiSession ()
withElection = do

  manifestPostBody <- liftIO manifestPostBodyIO

  -- We first create an election
  postJson "/election" electionPostBody

  -- Define the contests
  let contestPostBody = manifestPostBody "contest" "TestContestManifest.json"
  postJson "/manifest" contestPostBody

  -- Define the candidates for the contests
  let candidatePostBody = manifestPostBody "candidate" "TestCandidateManifest.json"
  postJson "/manifest" candidatePostBody

  return ()

withBallots :: WaiSession ()
withBallots = do
  testFile <- liftIO testFileIO
  let ballotPath i = testFile $ "test-ballots" </> "ballot-" ++ show i

  let ballotSrcPaths = map ballotPath [1 .. 100]
      ballotPostBody = [json|{
        electionId: 1,
        filePaths: #{ballotSrcPaths}
      }|]
  postJson "/ballot" ballotPostBody

  return ()
