module OpenRLA.Controller where

import           Data.Aeson (Object)
import           Data.Aeson.Types (Parser, parseMaybe)
import           Network.HTTP.Types.Status (badRequest400)
import           Web.Scotty (ActionM, jsonData, status)

import           OpenRLA.Types (State)


type Controller = State -> ActionM ()

parseThen :: (Object -> Parser a) -> (a -> ActionM ()) -> ActionM ()
parseThen p f = do
  o <- jsonData
  case parseMaybe p o of
    Nothing -> status badRequest400
    Just a  -> f a
