module OpenRLA.Controller.Audit where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, (.:))
import           Data.Aeson.Types (Parser)
import           Data.Text (Text)
import           Network.HTTP.Types.Status (notFound404)
import           Web.Scotty (json, param, status)

import           OpenRLA.Controller
import qualified OpenRLA.Statement.Audit as AuSt
import qualified OpenRLA.Statement.Ballot as BalSt
import           OpenRLA.Types (Audit(..), AuditMark(..), State(..))


index :: Controller
index State { conn } = liftIO (AuSt.index conn) >>= json

type CreateData = (Integer, Text, Double)

create :: Controller
create State { conn } = parseThen createP createCb
  where
    createCb args = do
      audit <- liftIO $ AuSt.create conn args
      json audit

createP :: Object -> Parser CreateData
createP o = do
  elId      <- o .: "electionId"
  date      <- o .: "date"
  riskLimit <- o .: "riskLimit"
  return (elId, date, riskLimit)

getById :: Controller
getById State { conn } = do
  auId <- param "id"
  audit <- liftIO $ AuSt.getById conn auId
  maybe (status notFound404) json audit

setById :: Controller
setById State { conn } = parseThen setByIdP setByIdCb
  where
    setByIdCb (auDate, auElectionId, auRiskLimit) = do
      auId <- param "id"
      let audit = Audit { .. }
      liftIO $ AuSt.setById conn audit

setByIdP :: Object -> Parser (Text, Integer, Double)
setByIdP o = do
  auDate       <- o .: "date"
  auElectionId <- o .: "electionId"
  auRiskLimit  <- o .: "riskLimit"
  return (auDate, auElectionId, auRiskLimit)

getActive :: Controller
getActive State { conn } = do
  audit <- liftIO $ AuSt.getActive conn
  maybe (status notFound404) json audit

setActive :: Controller
setActive State { conn } = parseThen (.: "auditId") setActiveCb
  where
    setActiveCb auId = liftIO (AuSt.setActive conn auId) >>= json

indexMarks :: Controller
indexMarks State { conn } = do
  auId <- param "id"
  marks <- liftIO $ AuSt.indexMarks conn auId
  json marks

createMarks :: Controller
createMarks State { conn } = parseThen createMarksP createMarksCb
  where
    createMarksCb (amBallotId, amContestId, amCandidateId) = do
      amAuditId <- param "id"
      let auditMark = AuditMark { .. }
      liftIO $ AuSt.createMark conn auditMark

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
    sampleId <- AuSt.currentSampleId conn auId
    BalSt.getById conn sampleId
  json sample
