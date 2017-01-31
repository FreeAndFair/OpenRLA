module JsonTestSupport where

import qualified Data.Aeson as A
import           Data.Aeson (Value(..))
import           Data.HashMap.Lazy ((!))
import           Data.Maybe (fromJust)
import           Data.Text (Text)

import           Network.Wai.Test (SResponse(..), simpleBody)


jsonLength :: Value -> Integer
jsonLength (Array a) = (fromIntegral . length) a

(.!) :: Value -> Text -> Value
Object o .! k = o ! k

decodeBody :: SResponse -> Value
decodeBody SResponse { simpleBody } = fromJust $ A.decode simpleBody
