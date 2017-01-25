module IntegrationTests.Election where

import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Test.Tasty.Hspec

import           TestSupport


spec :: Spec
spec = do
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

  around withApp $ context "Creating a second election" $ do
    let postBodyA = [json|
            {
              date: "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)",
              title: "Election A"
            }
          |]
        postBodyB = [json|
            {
              date: "Wed Jan 02 2016 13:02:35 GMT-0000 (UTC)",
              title: "Election B"
            }
          |]

    it "should set the newest election as active" $ do
      post "/election" postBodyA
      post "/election" postBodyB

      get "/election/active" `shouldRespondWith` [json|
          {
            date: "Wed Jan 02 2016 13:02:35 GMT-0000 (UTC)",
            active: true,
            id: 2,
            title: "Election B"
          }
        |]

    it "should return the distinct created elections" $ do
      post "/election" postBodyA `shouldRespondWith` [json|
          {
            date: "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)",
            active: true,
            id: 1,
            title: "Election A"
          }
        |]
      post "/election" postBodyB `shouldRespondWith` [json|
          {
            date: "Wed Jan 02 2016 13:02:35 GMT-0000 (UTC)",
            active: true,
            id: 2,
            title: "Election B"
          }
        |]

    it "should create two elections" $ do
      post "/election" postBodyA
      post "/election" postBodyB

      get "/election" `shouldRespondWith` [json|
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

      get "/election/1" `shouldRespondWith` [json|
          {
            date: "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)",
            active: false,
            id: 1,
            title: "Election A"
          }
        |]
      get "/election/2" `shouldRespondWith` [json|
          {
            date: "Wed Jan 02 2016 13:02:35 GMT-0000 (UTC)",
            active: true,
            id: 2,
            title: "Election B"
          }
        |]
