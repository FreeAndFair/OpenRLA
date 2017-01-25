module IntegrationTests where

import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Test.Tasty.Hspec

import           TestSupport


integrationSpec :: Spec
integrationSpec = do
  around withApp $ context "Initial state" $ do
    it "should have no elections" $ do
      get "/election" `shouldRespondWith` "[]"

    it "should have no active election" $ do
      get "/election/active" `shouldRespondWith` 404

    it "should have no ballots" $ do
      get "/ballot" `shouldRespondWith` "[]"

    it "should have no audits" $ do
      get "/audit" `shouldRespondWith` "[]"

  around withApp $ context "Creating a first election" $ do
    let postBody = [json|
                     {
                       title: "POTUS 2016",
                       date: "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)"
                     }
                   |]
        electionBody = [json|
            {
              date: "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)",
              active: true,
              id: 1,
              title: "POTUS 2016"
            }
          |]

    it "should create the election" $ do
      let expectedBody = [json|
          [{
            date: "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)",
            active: true,
            id: 1,
            title: "POTUS 2016"
          }]
        |]
      post "/election" postBody
      get "/election" `shouldRespondWith` expectedBody

    it "should return the new election" $ do
      post "/election" postBody `shouldRespondWith` electionBody

    it "should expose the new election via its id" $ do
      post "/election" postBody
      get "/election/1" `shouldRespondWith` electionBody

    it "should set the new (and only) election as active" $ do
      post "/election" postBody
      get "/election/active" `shouldRespondWith` electionBody
