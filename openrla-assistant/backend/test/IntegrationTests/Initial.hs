module IntegrationTests.Initial where

import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import           TestSupport


spec :: Spec
spec = do
  around withApp $ context "Initial state" $ do
    it "should have no elections" $ do
      get "/election" `shouldRespondWith` "[]"

    it "should have no active election" $ do
      get "/election/active" `shouldRespondWith` 404

    it "should have no ballots" $ do
      get "/ballot" `shouldRespondWith` "[]"

    it "should have no audits" $ do
      get "/audit" `shouldRespondWith` "[]"
