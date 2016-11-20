module Controller where

import           Data.Aeson (Object)
import           Data.Aeson.Types (Parser, parseMaybe)
import           Database.SQLite.Simple (Connection)
import           Web.Scotty (ActionM, jsonData)


type Controller = Connection -> ActionM ()

parseThen :: (Object -> Parser a) -> (a -> ActionM ()) -> ActionM ()
parseThen p f = do
  o <- jsonData
  case parseMaybe p o of
    Nothing -> return ()
    Just a  -> f a
