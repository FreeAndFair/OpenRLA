module JsonTestSupport where

import           Data.Aeson (Value(..))


jsonLength :: Value -> Maybe Integer
jsonLength v = case v of
  Array a -> Just $ (fromIntegral . length) a
  _       -> Nothing
