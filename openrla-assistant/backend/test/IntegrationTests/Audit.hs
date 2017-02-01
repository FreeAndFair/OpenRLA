module IntegrationTests.Audit where

import           Data.Aeson (Value(..))

import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import qualified IntegrationTests.Fixture as Fixture
import           JsonTestSupport
import           TestSupport


date :: String
date = "Tue Jan 01 2016 12:34:56 GMT-0000 (UTC)"

electionPostBody :: Value
electionPostBody = [json|{
  title: "The Election",
  date: #{date}
}|]

auditPostBodyA :: Value
auditPostBodyA = [json|{
  electionId: 1,
  date: #{date},
  riskLimit: 0.1,
  contests: [1001, 1003]
}|]

auditJsonA :: Value
auditJsonA = [json|{
  id: 1,
  electionId: 1,
  date: #{date},
  riskLimit: 0.1,
  sampled: [],
  contests: [
    {
      id: 1001,
      statistic: 1.0
    },
    {
      id: 1003,
      statistic: 1.0
    }
  ]
}|]

auditPostBodyB :: Value
auditPostBodyB = [json|{
  electionId: 1,
  date: #{date},
  riskLimit: 0.02,
  contests: [1002]
}|]

auditJsonB :: Value
auditJsonB = [json|{
  id: 2,
  electionId: 1,
  date: #{date},
  riskLimit: 0.02,
  sampled: [],
  contests: [
    {
      id: 1002,
      statistic: 1.0
    }
  ]
}|]

spec :: Spec
spec = do
  around withApp $ context "Auditing" $ do
    it "should create an active audit" $ do
      Fixture.withElection
      Fixture.withBallots

      get "/audit" `shouldRespondWith` "[]"
      get "/audit/1" `shouldRespondWith` 404

      auditCreateResp <- postJson "/audit" auditPostBodyA

      return auditCreateResp `shouldRespondWith` 200

      let createBody = decodeBody auditCreateResp
      liftIO $ createBody `shouldBe` auditJsonA

      auditIndexResp <- get "/audit"

      return auditIndexResp `shouldRespondWith` 200

      let indexBody = decodeBody auditIndexResp
      liftIO $ indexBody `shouldBe` [json|[#{auditJsonA}]|]

      auditActiveResp <- get "/audit/active"

      return auditActiveResp `shouldRespondWith` 200

      let activeBody = decodeBody auditActiveResp
      liftIO $ activeBody `shouldBe` auditJsonA

      auditByIdResp <- get "/audit/1"

      return auditByIdResp `shouldRespondWith` 200

      let byIdBody = decodeBody auditByIdResp
      liftIO $ byIdBody `shouldBe` auditJsonA

      get "/audit/666" `shouldRespondWith` 404

    it "should handle multiple audits" $ do
      Fixture.withElection
      Fixture.withBallots

      postJson "/audit" auditPostBodyA
      auditCreateResp <- postJson "/audit" auditPostBodyB

      let createBody = decodeBody auditCreateResp
      liftIO $ createBody `shouldBe` auditJsonB

      auditIndexResp <- get "/audit"

      return auditIndexResp `shouldRespondWith` 200

      let indexBody = decodeBody auditIndexResp
      liftIO $ indexBody `shouldBe` [json|[
        #{auditJsonA},
        #{auditJsonB}
      ]|]

      auditActiveResp <- get "/audit/active"

      return auditActiveResp `shouldRespondWith` 200

      let activeBody = decodeBody auditActiveResp
      liftIO $ activeBody `shouldBe` auditJsonB

      auditByIdResp <- get "/audit/1"

      return auditByIdResp `shouldRespondWith` 200

      let byIdBody = decodeBody auditByIdResp
      liftIO $ byIdBody `shouldBe` auditJsonA

      auditByIdResp <- get "/audit/2"

      return auditByIdResp `shouldRespondWith` 200

      let byIdBody = decodeBody auditByIdResp
      liftIO $ byIdBody `shouldBe` auditJsonB

      get "/audit/666" `shouldRespondWith` 404
