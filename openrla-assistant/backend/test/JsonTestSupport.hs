module JsonTestSupport where

import qualified Data.Aeson as A
import           Data.Aeson (Value(..))
import           Data.HashMap.Lazy ((!))
import           Data.Maybe (fromJust)
import           Data.Text (Text)

import           Network.Wai.Test (SResponse(..), simpleBody)
import           Test.Hspec.Wai
import           Test.Tasty.Hspec


jsonLength :: Value -> Integer
jsonLength (Array a) = (fromIntegral . length) a

(.!) :: Value -> Text -> Value
Object o .! k = o ! k

decodeBody :: SResponse -> Value
decodeBody SResponse { simpleBody } = fromJust $ A.decode simpleBody

decodeBody' :: A.FromJSON a => SResponse -> a
decodeBody' SResponse { simpleBody } = fromJust $ A.decode simpleBody

bodyShouldBe :: SResponse -> Value -> WaiSession ()
bodyShouldBe resp val = liftIO $ decodeBody resp `shouldBe` val

getBodyId :: SResponse -> Integer
getBodyId resp = truncate balId
  where body = decodeBody resp
        Number balId = body .! "id"
