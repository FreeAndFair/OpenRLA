module OpenRLA.Controller.Audit where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, (.:), (.:?), (.!=), (.=), object)
import           Data.Aeson.Types (Parser)
import           Data.Text (Text)
import           Web.Scotty (json)

import           OpenRLA.Controller
import qualified OpenRLA.Statement.Audit as St
import           OpenRLA.Types (State(..))


index :: Controller
index State { conn } = parseThen indexP indexCb
  where
    indexCb args = do
      rows <- liftIO $ St.index conn args
      json rows

indexP :: Object -> Parser (Integer, Integer)
indexP o = do
  offset <- o .:? "offset" .!= 0
  limit  <- o .:? "limit"  .!= 20
  let boundedLimit = min limit 20
  return (boundedLimit, offset)

type CreateData = (Integer, Text, Double)

create :: Controller
create State { conn } = parseThen createP createCb
  where
    createCb args@(elId, date, riskLimit) = do
      auditId <- liftIO $ St.create conn args
      json $ object [ "id"         .= auditId
                    , "date"       .= date
                    , "electionId" .= elId
                    , "riskLimit"  .= riskLimit
                    ]

createP :: Object -> Parser CreateData
createP o = do
  elId      <- o .: "electionId"
  date      <- o .: "date"
  riskLimit <- o .: "riskLimit"
  return (elId, date, riskLimit)

getById :: Controller
getById State { conn } = parseThen (.: "auditId") cb
  where
    cb auditId = liftIO (St.getAuditById conn auditId) >>= json

setById :: Controller
setById = undefined

getActive :: Controller
getActive State { conn } = liftIO (St.getActiveAudit conn) >>= json

setActive :: Controller
setActive State { conn } = parseThen (.: "auditId") cb
  where
    cb auditId = liftIO (St.setActiveAudit conn auditId) >>= json

indexMarks :: Controller
indexMarks = undefined

createMark :: Controller
createMark = undefined
