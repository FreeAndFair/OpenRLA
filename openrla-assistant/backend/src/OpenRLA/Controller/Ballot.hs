module OpenRLA.Controller.Ballot where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.:), (.=), object)
import           Data.Maybe (maybe)
import           Web.Scotty (json)

import           OpenRLA.Controller
import qualified OpenRLA.Statement.Ballot as St
import           OpenRLA.Types (State(..))


index :: Controller
index = undefined

create :: Controller
create = undefined

getById :: Controller
getById State { conn } = parseThen (.: "ballotId") cb
  where
    cb ballotId = do
      res <- liftIO (St.getBallotPathById conn ballotId)
      let pairs = maybe [] (\p -> [ "filePath" .= p ]) res
      json $ object pairs

setById :: Controller
setById = undefined
