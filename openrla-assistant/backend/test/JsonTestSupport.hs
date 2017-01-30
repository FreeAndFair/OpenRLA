module JsonTestSupport where

import qualified Data.Aeson as A
import           Data.Aeson (Value(..))
import           Data.Maybe (fromJust)

import           Network.Wai.Test (SResponse(..), simpleBody)


jsonLength :: Value -> Maybe Integer
jsonLength v = case v of
  Array a -> Just $ (fromIntegral . length) a
  _       -> Nothing

decodeBody :: Monad m => m SResponse -> m Value
decodeBody resp = do
  SResponse { simpleBody } <- resp
  return $ fromJust $ A.decode simpleBody
