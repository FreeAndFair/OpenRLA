module Controller.Election where

import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.:), (.=), object, toJSON)
import           Web.Scotty (json)

import           Controller
import qualified Statement as St
import           Types (State(..))


index :: Controller
index State { conn } = do
  let offset = 0
      limit  = 20
  rows <- liftIO (St.getElectionIndex conn offset limit)
  let cb (id_, title, date, _)
        = return $ object [ "id"    .= id_
                          , "title" .= title
                          , "date"  .= date
                          ]
  forM rows cb >>= json

create :: Controller
create = undefined

getById :: Controller
getById State { conn } = parseThen (.: "electionId") cb
  where
    cb elId = do
      election <- liftIO (St.getElectionById conn elId)
      let res = maybe (object []) toJSON election
      json res

setById :: Controller
setById = undefined

getActive :: Controller
getActive = undefined
-- getActive State { conn } = liftIO (St.getActiveElection conn) >>= json

setActive :: Controller
setActive State { conn } = parseThen (.: "electionId") cb
  where
    cb electionId = liftIO (St.setActiveElection conn electionId) >>= json
