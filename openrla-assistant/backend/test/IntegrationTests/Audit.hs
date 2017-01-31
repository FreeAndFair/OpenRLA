module IntegrationTests.Audit where

import           Data.Aeson (Value(..))

import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import           JsonTestSupport
import           TestSupport


date :: String
date = "Tue Jan 01 2016 12:01:23 GMT-0000 (UTC)"

electionPostBody :: Value
electionPostBody = [json|{
  title: "POTUS 2016",
  date: #{date}
}|]

auditPostBody :: Value
auditPostBody = [json|{
  electionId: 1,
  date: #{date},
  riskLimit: 0.1
}|]

auditJson :: Value
auditJson = [json|{
  id: 1,
  electionId: 1,
  date: #{date},
  riskLimit: 0.1
}|]

spec :: Spec
spec = do
  around withApp $ context "Auditing" $ do
    it "should work" $ do
      postJson "/election" electionPostBody `shouldRespondWith` 200

      get "/audit" `shouldRespondWith` "[]"

      auditCreateResp <- postJson "/audit" auditPostBody

      return auditCreateResp `shouldRespondWith` 200

      let createBody = decodeBody auditCreateResp
      liftIO $ createBody `shouldBe` auditJson

      auditIndexResp <- get "/audit"

      return auditIndexResp `shouldRespondWith` 200

      let indexBody = decodeBody auditIndexResp
      liftIO $ indexBody `shouldBe` [json|[#{auditJson}]|]
