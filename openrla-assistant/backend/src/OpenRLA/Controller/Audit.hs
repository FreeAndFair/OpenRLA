module OpenRLA.Controller.Audit where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, (.:), (.:?), (.!=))
import           Data.Aeson.Types (Parser)
import           Data.Text (Text)
import           Network.HTTP.Types.Status (notFound404)
import           Web.Scotty (json, param, status)

import           OpenRLA.Controller
import qualified OpenRLA.Statement.Audit as St
import qualified OpenRLA.Statement.Ballot as BalSt
import           OpenRLA.Types (Audit(..), AuditMark(..), State(..))


index :: Controller
index State { conn } = liftIO (St.index conn) >>= json

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
setActive State { conn } = parseThen (.: "auditId") setActiveCb
  where
    setActiveCb auId = liftIO (St.setActive conn auId) >>= json

indexMarks :: Controller
indexMarks State { conn } = do
  auId <- param "id"
  marks <- liftIO $ St.indexMarks conn auId
  json marks

createMarks :: Controller
createMarks State { conn } = parseThen createMarksP createMarksCb
  where
    createMarksCb (amBallotId, amContestId, amCandidateId) = do
      amAuditId <- param "id"
      let auditMark = AuditMark { .. }
      liftIO $ St.createMark conn auditMark

createMarksP :: Object -> Parser (Integer, Integer, Integer)
createMarksP o = do
  ballotId    <- o .: "ballotId"
  contestId   <- o .: "contestId"
  candidateId <- o .: "candidateId"
  return (ballotId, contestId, candidateId)

currentSample :: Controller
currentSample State { conn } = do
  auId <- param "id"
  sample <- liftIO $ do
    sampleId <- St.currentSampleId conn auId
    BalSt.getById conn sampleId
  json sample
