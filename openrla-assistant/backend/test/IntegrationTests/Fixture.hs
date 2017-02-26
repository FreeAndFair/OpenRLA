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

withOneBallot :: WaiSession ()
withOneBallot = do
  testFile <- liftIO testFileIO
  let ballotPath i = testFile $ "test-ballots" </> "ballot-" ++ show i

  let ballotSrcPaths = [ballotPath 1]
      ballotPostBody = [json|{
        electionId: 1,
        filePaths: #{ballotSrcPaths}
      }|]
  postJson "/ballot" ballotPostBody

  return ()

withOutcomes :: WaiSession ()
withOutcomes = do
  let outcomeJson1 = [json|{
        id: 1001,
        shares: [
          { id: 1, share: 0.6 },
          { id: 2, share: 0.3 },
          { id: 3, share: 0.1 }
        ]
      }|]
      outcomeJson2 = [json|{
        id: 1002,
        shares: [
          { id: 4, share: 0.8 },
          { id: 5, share: 0.2 }
        ]
      }|]
      outcomeJson3 = [json|{
        id: 1003,
        shares: [
          { id: 6, share: 1.0 }
        ]
      }|]

  postJson "/election/1/outcome" outcomeJson1
  postJson "/election/1/outcome" outcomeJson2
  postJson "/election/1/outcome" outcomeJson3

  return ()
