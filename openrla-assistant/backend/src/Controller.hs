module Controller where

import           Data.Aeson (Object)
import           Data.Aeson.Types (Parser, parseMaybe)
import           Web.Scotty (ActionM, jsonData)

import           Types (State)


type Controller = State -> ActionM ()

parseThen :: (Object -> Parser a) -> (a -> ActionM ()) -> ActionM ()
parseThen p f = do
  o <- jsonData
  case parseMaybe p o of
    Nothing -> return ()
    Just a  -> f a
