module IntegrationTests where

import           Test.Hspec.Wai
import           Test.Tasty.Hspec

import           TestSupport

import           Network.Wai (Application)
import           Web.Scotty (scottyApp)

import           OpenRLA (mkApp)


appIO :: IO Application
appIO = do
  state <- testState
  let app = mkApp state
  scottyApp app

integrationSpec :: Spec
integrationSpec = with appIO $ do
  context "initial state" $ do
    it "should have no elections to start with" $ do
      get "/election" `shouldRespondWith` "[]"
