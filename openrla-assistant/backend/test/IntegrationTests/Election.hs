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
        electionBody = [json|{
          date: #{date},
          active: true,
          id: 1,
          title: "POTUS 2016"
        }|]

    it "should create the election" $ do
      let expectedBody = [matchJson|[#{electionBody}]|]
      post "/election" postBodyS
      get "/election" `shouldRespondWith` expectedBody

    it "should return the new election" $ do
      post "/election" postBodyS `shouldRespondWithJson` electionBody

    it "should expose the new election via its id" $ do
      post "/election" postBodyS
      get "/election/1" `shouldRespondWithJson` electionBody

    it "should set the new (and only) election as active" $ do
      post "/election" postBodyS
      get "/election/active" `shouldRespondWithJson` electionBody

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
        mkElectionJsonA = \active -> [json|{
          date: #{dateA},
          active: #{active},
          id: 1,
          title: "Election A"
        }|]
        mkElectionJsonB = \active -> [json|{
          date: #{dateB},
          active: #{active},
          id: 2,
          title: "Election B"
        }|]

    it "should set the newest election as active" $ do
      post "/election" postBodyA
      post "/election" postBodyB

      get "/election/active" `shouldRespondWithJson` (mkElectionJsonB True)

    it "should return the distinct created elections" $ do
      post "/election" postBodyA `shouldRespondWithJson` (mkElectionJsonA True)
      post "/election" postBodyB `shouldRespondWithJson` (mkElectionJsonB True)

    it "should create two elections" $ do
      post "/election" postBodyA
      post "/election" postBodyB

      get "/election" `shouldRespondWithJson` [json|[
        #{mkElectionJsonA False},
        #{mkElectionJsonB True}
      ]|]

    it "should make both elections accessible via id" $ do
      post "/election" postBodyA
      post "/election" postBodyB

      get "/election/1" `shouldRespondWithJson` (mkElectionJsonA False)
      get "/election/2" `shouldRespondWithJson` (mkElectionJsonB True)
