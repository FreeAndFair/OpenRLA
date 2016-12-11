module OpenRLA.Controller.Election where

import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, (.:), (.:?), (.=), object)
import           Data.Aeson.Types (Parser)
import           Data.Maybe (maybe)
import           Data.Text (Text)
import           Network.HTTP.Types.Status (notFound404)
import           Web.Scotty (json, param, status)

import           OpenRLA.Controller
import qualified OpenRLA.Statement.Election as St
import           OpenRLA.Types (Election(..), State(..))


index :: Controller
index State { conn } = parseThen indexP indexCb
  where
    indexCb (offset, limit) = do
      rows <- liftIO $ St.index conn offset limit
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
      elId <- liftIO $ St.create conn elTitle elDate
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
getById State { conn } = do
  elId <- param "id"
  res <- liftIO (St.getById conn elId)
  maybe (status notFound404) json res

setById :: Controller
setById State { conn } = parseThen setByIdP setByIdCb
  where
    setByIdCb (elTitle, elDate, elActive) = do
      elId <- param "id"
      let election = Election { .. }
      liftIO $ St.setById conn election

setByIdP :: Object -> Parser (Text, Text, Bool)
setByIdP o = do
  title  <- o .: "title"
  date   <- o .: "date"
  active <- o .: "active"
  return (title, date, active)

getActive :: Controller
getActive State { conn } = liftIO (St.getActive conn) >>= json

setActive :: Controller
setActive State { conn } = parseThen (.: "electionId") cb
  where
    cb eId = liftIO (St.setActive conn eId) >>= json
