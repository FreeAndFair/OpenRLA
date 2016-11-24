module Controller.Election where

import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.:), (.=), object, toJSON)
import           Web.Scotty (json)

import           Controller
import qualified Query as Q
import           Types (State(..))


index :: Controller
index State { conn } = do
  let offset = 0
      limit  = 20
  rows <- liftIO (Q.getElectionIndex conn offset limit)
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
      election <- liftIO (Q.getElectionById conn elId)
      let res = maybe (object []) toJSON election
      json res

setById :: Controller
setById = undefined

getActive :: Controller
getActive = undefined
-- getActive State { conn } = liftIO (Q.getActiveElection conn) >>= json

setActive :: Controller
setActive State { conn } = parseThen (.: "electionId") cb
  where
    cb electionId = liftIO (Q.setActiveElection conn electionId) >>= json
