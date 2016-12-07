module OpenRLA.Controller.Election where

import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, (.:), (.:?), (.=), object, toJSON)
import           Data.Aeson.Types (Parser)
import           Data.Maybe (maybe)
import           Data.Text (Text)
import           Web.Scotty (json)

import           OpenRLA.Controller
import qualified OpenRLA.Statement.Election as St
import           OpenRLA.Types (State(..))


index :: Controller
index State { conn } = parseThen indexP indexCb
  where
    indexCb (offset, limit) = do
      rows <- liftIO $ St.getElectionIndex conn offset limit
      let rowCb (elId, title, date, _)
            = return $ object [ "id"    .= elId
                              , "title" .= title
                              , "date"  .= date
                              ]
      forM rows rowCb >>= json

indexP :: Object -> Parser (Integer, Integer)
indexP o = do
  offset' <- o .:? "offset"
  limit'  <- o .:? "limit"
  let offset = maybe 0  id       offset'
      limit  = maybe 20 (min 20) limit'
  return (offset, limit)

create :: Controller
create State { conn } = parseThen createP createCb
  where
    createCb (elTitle, elDate) = do
      elId <- liftIO $ St.createElection conn elTitle elDate
      json $ object [ "id"    .= elId
                    , "title" .= elTitle
                    , "date"  .= elDate
                    ]

createP :: Object -> Parser (Text, Text)
createP o = do
  title <- o .: "title"
  date  <- o .: "date"
  return (title, date)

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
getActive State { conn } = liftIO (St.getActiveElection conn) >>= json

setActive :: Controller
setActive State { conn } = parseThen (.: "electionId") cb
  where
    cb eId = liftIO (St.setActiveElection conn eId) >>= json
