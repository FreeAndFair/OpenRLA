module IntegrationTests.Election where

import qualified Data.Aeson as A
import           Data.ByteString (ByteString)

import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import           TestSupport


spec :: Spec
spec = do
  around withApp $ context "Creating a first election" $ do
    let date = "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)" :: String
    let postBody = [json|{
          title: "POTUS 2016",
          date: #{date}
        }|]
        postBodyS = A.encode postBody
        electionBody = [json|
            {
              date: #{date},
              active: true,
              id: 1,
              title: "POTUS 2016"
            }
          |]
        electionBodyMatcher = [matchJson|#{electionBody}|]

    it "should create the election" $ do
      let expectedBody = [matchJson|[#{electionBody}]|]
      post "/election" postBodyS
      get "/election" `shouldRespondWith` expectedBody

    it "should return the new election" $ do
      post "/election" postBodyS `shouldRespondWith` electionBodyMatcher

    it "should expose the new election via its id" $ do
      post "/election" postBodyS
      get "/election/1" `shouldRespondWith` electionBodyMatcher

    it "should set the new (and only) election as active" $ do
      post "/election" postBodyS
      get "/election/active" `shouldRespondWith` electionBodyMatcher

  around withApp $ context "Creating a second election" $ do
    let dateA = "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)" :: String
        postBodyA = [matchJson|{
          date: #{dateA},
          title: "Election A"
        }|]
        dateB = "Wed Jan 02 2016 13:02:35 GMT-0000 (UTC)" :: String
        postBodyB = [matchJson|{
          date: #{dateB},
          title: "Election B"
        }|]

    it "should set the newest election as active" $ do
      post "/election" postBodyA
      post "/election" postBodyB

      get "/election/active" `shouldRespondWith` [matchJson|{
        date: "Wed Jan 02 2016 13:02:35 GMT-0000 (UTC)",
        active: true,
        id: 2,
        title: "Election B"
      }|]

    it "should return the distinct created elections" $ do
      post "/election" postBodyA `shouldRespondWith` [matchJson|{
        date: #{dateA},
        active: true,
        id: 1,
        title: "Election A"
      }|]
      post "/election" postBodyB `shouldRespondWith` [matchJson|{
        date: #{dateB},
        active: true,
        id: 2,
        title: "Election B"
      }|]

    it "should create two elections" $ do
      post "/election" postBodyA
      post "/election" postBodyB

      get "/election" `shouldRespondWith` [matchJson|
          [
            {
              date: "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)",
              active: false,
              id: 1,
              title: "Election A"
            },
            {
              date: "Wed Jan 02 2016 13:02:35 GMT-0000 (UTC)",
              active: true,
              id: 2,
              title: "Election B"
            }
          ]
        |]

    it "should make both elections accessible via id" $ do
      post "/election" postBodyA
      post "/election" postBodyB

      get "/election/1" `shouldRespondWith` [matchJson|
          {
            date: "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)",
            active: false,
            id: 1,
            title: "Election A"
          }
        |]
      get "/election/2" `shouldRespondWith` [matchJson|
          {
            date: "Wed Jan 02 2016 13:02:35 GMT-0000 (UTC)",
            active: true,
            id: 2,
            title: "Election B"
          }
        |]
