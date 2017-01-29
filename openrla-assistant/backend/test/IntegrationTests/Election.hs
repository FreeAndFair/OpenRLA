module IntegrationTests.Election where

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
        electionBody = [json|{
          date: #{date},
          active: true,
          id: 1,
          title: "POTUS 2016"
        }|]

    it "should create the election" $ do
      let expectedBody = [matchJson|[#{electionBody}]|]
      postJson "/election" postBody
      get "/election" `shouldRespondWith` expectedBody

    it "should return the new election" $ do
      postJson "/election" postBody `shouldRespondWithJson` electionBody

    it "should expose the new election via its id" $ do
      postJson "/election" postBody
      get "/election/1" `shouldRespondWithJson` electionBody

    it "should set the new (and only) election as active" $ do
      postJson "/election" postBody
      get "/election/active" `shouldRespondWithJson` electionBody

  around withApp $ context "Creating a second election" $ do
    let dateA = "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)" :: String
        postBodyA = [json|{
          date: #{dateA},
          title: "Election A"
        }|]
        dateB = "Wed Jan 02 2016 13:02:35 GMT-0000 (UTC)" :: String
        postBodyB = [json|{
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
      postJson "/election" postBodyA
      postJson "/election" postBodyB

      get "/election/active" `shouldRespondWithJson` (mkElectionJsonB True)

    it "should return the distinct created elections" $ do
      postJson "/election" postBodyA `shouldRespondWithJson` (mkElectionJsonA True)
      postJson "/election" postBodyB `shouldRespondWithJson` (mkElectionJsonB True)

    it "should create two elections" $ do
      postJson "/election" postBodyA
      postJson "/election" postBodyB

      get "/election" `shouldRespondWithJson` [json|[
        #{mkElectionJsonA False},
        #{mkElectionJsonB True}
      ]|]

    it "should make both elections accessible via id" $ do
      postJson "/election" postBodyA
      postJson "/election" postBodyB

      get "/election/1" `shouldRespondWithJson` (mkElectionJsonA False)
      get "/election/2" `shouldRespondWithJson` (mkElectionJsonB True)
