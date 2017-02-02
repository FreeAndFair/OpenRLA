module IntegrationTests.Election where

import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import qualified IntegrationTests.Fixture as Fixture
import           JsonTestSupport
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


  around withApp $ context "Election outcomes" $ do
    it "should allow associating contest outcomes with the election" $ do
      Fixture.withElection

      get "/election/1/outcome" `shouldRespondWithJson` [json|[]|]

      let outcomeJson1 = [json|{
            id: 1001,
            shares: [
              {
                id: 1,
                share: 0.6
              },
              {
                id: 2,
                share: 0.3
              },
              {
                id: 3,
                share: 0.1
              }
            ]
          }|]
          outcomeJson2 = [json|{
            id: 1002,
            shares: [
              {
                id: 4,
                share: 0.8
              },
              {
                id: 5,
                share: 0.2
              }
            ]
          }|]
          outcomeJson3 = [json|{
            id: 1003,
            shares: [
              {
                id: 6,
                share: 1.0
              }
            ]
          }|]

      postResp1 <- postJson "/election/1/outcome" outcomeJson1
      return postResp1 `shouldRespondWith` 200
      postResp1 `bodyShouldBe` outcomeJson1

      postResp2 <- postJson "/election/1/outcome" outcomeJson2
      return postResp2 `shouldRespondWith` 200
      postResp2 `bodyShouldBe` outcomeJson2

      postResp3 <- postJson "/election/1/outcome" outcomeJson3
      return postResp3 `shouldRespondWith` 200
      postResp3 `bodyShouldBe` outcomeJson3

      indexResp <- get "/election/1/outcome"
      return indexResp `shouldRespondWith` 200
      indexResp `bodyShouldBe` [json|[
        #{outcomeJson1},
        #{outcomeJson2},
        #{outcomeJson3}
      ]|]
