module OpenRLA.Controller.Audit where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, (.:), (.:?), (.!=))
import           Data.Aeson.Types (Parser)
import           Data.Text (Text)
import           Network.HTTP.Types.Status (notFound404)
import           Web.Scotty (json, param, status)

import           OpenRLA.Controller
import qualified OpenRLA.Statement.Audit as St
import           OpenRLA.Types (Audit(..), AuditMark(..), State(..))


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
    createCb args = do
      audit <- liftIO $ St.create conn args
      json audit

createP :: Object -> Parser CreateData
createP o = do
  elId      <- o .: "electionId"
  date      <- o .: "date"
  riskLimit <- o .: "riskLimit"
  return (elId, date, riskLimit)

getById :: Controller
getById State { conn } = parseThen (.: "auditId") getByIdCb
  where
    getByIdCb auditId = do
      res <- liftIO $ St.getById conn auditId
      maybe (status notFound404) json res

setById :: Controller
setById State { conn } = parseThen setByIdP setByIdCb
  where
    setByIdCb (auDate, auElectionId, auRiskLimit) = do
      auId <- param "id"
      let audit = Audit { .. }
      liftIO $ St.setById conn audit

setByIdP :: Object -> Parser (Text, Integer, Double)
setByIdP o = do
  auDate       <- o .: "date"
  auElectionId <- o .: "electionId"
  auRiskLimit  <- o .: "riskLimit"
  return (auDate, auElectionId, auRiskLimit)

getActive :: Controller
getActive State { conn } = do
  res <- liftIO $ St.getActive conn
  maybe (status notFound404) json res

setActive :: Controller
setActive State { conn } = parseThen (.: "auditId") cb
  where
    cb auditId = liftIO (St.setActive conn auditId) >>= json

indexMarks :: Controller
indexMarks State { conn } = do
  auId <- param "id"
  marks <- liftIO $ St.indexMarks conn auId
  json marks

createMark :: Controller
createMark State { conn } = parseThen createMarkP createMarkCb
  where
    createMarkCb (amBallotId, amContestId, amCandidateId) = do
      amAuditId <- param "auditId"
      let auditMark = AuditMark { .. }
      liftIO $ St.createMark conn auditMark

createMarkP :: Object -> Parser (Integer, Integer, Integer)
createMarkP o = do
  ballotId    <- o .: "ballotId"
  contestId   <- o .: "contestId"
  candidateId <- o .: "candidateId"
  return (ballotId, contestId, candidateId)
