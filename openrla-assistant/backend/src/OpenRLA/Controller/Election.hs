module OpenRLA.Controller.Election where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, (.:), (.=), object)
import           Data.Aeson.Types (Parser)
import           Data.Maybe (maybe)
import           Data.Text (Text)
import           Network.HTTP.Types.Status (notFound404)
import           System.Random (randomRIO)
import           Web.Scotty (json, param, status)

import           OpenRLA.Controller
import qualified OpenRLA.Statement.Ballot as BalSt
import qualified OpenRLA.Statement.Election as St
import           OpenRLA.Types (Election(..), State(..))


index :: Controller
index State { conn } = liftIO (St.index conn) >>= json

create :: Controller
create State { conn } = parseThen createP createCb
  where
    createCb (elTitle, elDate) = do
      election <- liftIO $ St.create conn elTitle elDate
      json election

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
getActive State { conn } = do
  active <- liftIO $ St.getActive conn
  maybe (status notFound404) json active

setActive :: Controller
setActive State { conn } = parseThen (.: "electionId") cb
  where
    cb eId = liftIO (St.setActive conn eId) >>= json

sampleBallot :: Controller
sampleBallot State { conn } = do
  elId  <- param "id"
  ballot <- liftIO $ do
    count <- St.ballotCountForId conn elId
    offset <- randomRIO (0, count - 1)
    BalSt.getByOffset conn offset
  json ballot
