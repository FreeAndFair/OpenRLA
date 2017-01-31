module IntegrationTests.Manifest where

import           Data.Aeson (Value(..))
import           Data.HashMap.Lazy ((!))
import qualified System.Directory as Dir
import           System.FilePath ((</>))

import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import           JsonTestSupport
import           TestSupport


candidatePath :: FilePath
candidatePath = "dominion" </> "CandidateManifest.json"

contestPath :: FilePath
contestPath = "dominion" </> "ContestManifest.json"

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
  around withApp $ context "Uploading manifests" $ do
    it "should process them when uploaded in the correct order" $ do
      manifestPostBody <- liftIO manifestPostBodyIO

      -- When we have an election
      postJson "/election" electionPostBody

      -- We can upload a contest manifest
      let contestPostBody = manifestPostBody "contest" contestPath
          contestResp = postJson "/manifest" contestPostBody

      body <- decodeBody contestResp
      liftIO $ do
        let bodyData = case body of
              Object o -> o ! "data"
        (jsonLength bodyData) `shouldBe` (Just 15)

      -- We can upload a candidate manifest
      let candidatePostBody = manifestPostBody "candidate" candidatePath
          candidateResp = postJson "/manifest" candidatePostBody

      candidateResp `shouldRespondWith` 200

      body <- decodeBody candidateResp
      liftIO $ do
        let bodyData = case body of
              Object o -> o ! "data"
        (jsonLength bodyData) `shouldBe` (Just 20)

      -- And its fully-defined contests will be associated with the election
      let electionResp = get "/election/1/contest"
      body <- decodeBody electionResp
      liftIO $ body `shouldBe` [json|[
        {
          "description": "United States Senator - DEM",
          "id": 1001,
          "externalId": "600013282",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Michael Bennet",
              "id": 1,
              "externalId": "1",
              "contestId": 1001,
              "type": "Regular"
            }
          ]
        },
        {
          "description": "Representative to the 115th United States Congress - District 2 - DEM",
          "id": 1002,
          "externalId": "600013270",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Jared Polis",
              "id": 2,
              "externalId": "1",
              "contestId": 1002,
              "type": "Regular"
            },
            {
              "description": "Write-in",
              "id": 3,
              "externalId": "",
              "contestId": 1002,
              "type": "WriteIn"
            }
          ]
        },
        {
          "description": "Regent of the University of Colorado - At Large - DEM",
          "id": 1003,
          "externalId": "600013286",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Alice Madden",
              "id": 4,
              "externalId": "1",
              "contestId": 1003,
              "type": "Regular"
            }
          ]
        },
        {
          "description": "State Representative - District 13 - DEM",
          "id": 1004,
          "externalId": "600013336",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
               "description": "KC Becker",
               "id": 5,
               "externalId": "1",
               "contestId": 1004,
               "type": "Regular"
            }
          ]
        },
        {
          "description": "District Attorney - 5th Judicial District - DEM",
          "id": 1005,
          "externalId": "600013509",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Bruce Brown",
              "id": 6,
              "externalId": "1",
              "contestId": 1005,
              "type": "Regular"
            }
          ]
        },
        {
          "description": "County Commissioner-District 2 - DEM",
          "id": 1006,
          "externalId": "600014377",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Nancy E. Judge",
              "id": 7,
              "externalId": "1",
              "contestId": 1006,
              "type": "Regular"
            }
          ]
        },
        {
          "description": "County Commissioner-District 3 - DEM",
          "id": 1007,
          "externalId": "600014416",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Randy Wheelock",
              "id": 8,
              "externalId": "1",
              "contestId": 1007,
              "type": "Regular"
            }
          ]
        },
        {
          "description": "County Treasurer - DEM",
          "id": 1008,
          "externalId": "600014307",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Carol Lee",
              "id": 9,
              "externalId": "1",
              "contestId": 1008,
              "type": "Regular"
            }
          ]
        },
        {
          "description": "United States Senator - REP",
          "id": 1009,
          "externalId": "600013283",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Darryl Glenn",
              "id": 10,
              "externalId": "1",
              "contestId": 1009,
              "type": "Regular"
            },
            {
              "description": "Ryan L. Frazier",
              "id": 11,
              "externalId": "2",
              "contestId": 1009,
              "type": "Regular"
            },
            {
              "description": "Robert Blaha",
              "id": 12,
              "externalId": "3",
              "contestId": 1009,
              "type": "Regular"
            },
            {
              "description": "Jack Graham",
              "id": 13,
              "externalId": "4",
              "contestId": 1009,
              "type": "Regular"
            },
            {
              "description": "Jon Keyser",
              "id": 14,
              "externalId": "5",
              "contestId": 1009,
              "type": "Regular"
            },
            {
              "description": "Write-in",
              "id": 15,
              "externalId": "",
              "contestId": 1009,
              "type": "WriteIn"
            }
          ]
        },
        {
          "description": "Representative to the 115th United States Congress - District 2 - REP",
          "id": 1010,
          "externalId": "600013271",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Nicholas Morse",
              "id": 16,
              "externalId": "1",
              "contestId": 1010,
              "type": "Regular"
            }
          ]
        },
        {
          "description": "Regent of the University of Colorado - At Large - REP",
          "id": 1011,
          "externalId": "600013287",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Heidi Ganahl",
              "id": 17,
              "externalId": "1",
              "contestId": 1011,
              "type": "Regular"
            }
          ]
        },
        {
          "description": "State Representative - District 13 - REP",
          "id": 1012,
          "externalId": "600013337",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": []
        },
        {
          "description": "District Attorney - 5th Judicial District - REP",
          "id": 1013,
          "externalId": "600013510",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Bruce Carey",
              "id": 19,
              "externalId": "1",
              "contestId": 1013,
              "type": "Regular"
            }
          ]
        },
        {
          "description": "County Commissioner-District 2 - REP",
          "id": 1014,
          "externalId": "600014415",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Sean Wood",
              "id": 20,
              "externalId": "1",
              "contestId": 1014,
              "type": "Regular"
            }
          ]
        },
        {
          "description": "County Commissioner-District 3 - REP",
          "id": 1015,
          "externalId": "600014378",
          "voteFor": 1,
          "numRanks": 0,
          "candidates": [
            {
              "description": "Cindy Condon",
              "id": 21,
              "externalId": "1",
              "contestId": 1015,
              "type": "Regular"
            }
          ]
        }
      ]|]
