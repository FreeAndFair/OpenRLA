module OpenRLA.Controller.Audit where

import           Control.Monad (forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, (.:), Value(..))
import           Data.Aeson.QQ (aesonQQ)
import           Data.Aeson.Types (Parser)
import           Data.Function (on)
import           Data.List (groupBy, sortBy)
import           Data.Maybe (fromJust)
import           Database.SQLite.Simple (Connection)
import           Data.Text (Text)
import           Network.HTTP.Types.Status (notFound404)
import           Web.Scotty (json, param, status)

import           OpenRLA.Controller
import qualified OpenRLA.Statement.Audit as AuSt
import qualified OpenRLA.Statement.Ballot as BalSt
import qualified OpenRLA.Statement.Election as ElSt
import           OpenRLA.Types (
    Audit(..)
  , AuditMark(..)
  , Ballot(..)
  , State(..)
  )


mkJson :: Connection -> Audit -> IO Value
mkJson conn Audit { .. } = do
  contestData <- AuSt.getContestData conn auId
  let contests = [ [aesonQQ|{id: #{cId}, statistic: #{stat}}|]
                 | (cId, stat) <- contestData ]
  return [aesonQQ|{
    id: #{auId},
    electionId: #{auElectionId},
    date: #{auDate},
    riskLimit: #{auRiskLimit},
    contests: #{contests}
  }|]

index :: Controller
index State { conn } = do
  rows <- liftIO $ AuSt.index conn
  objects <- liftIO $ forM rows (mkJson conn)
  json objects

type CreateData = (Integer, Text, Double, [Integer])

create :: Controller
create State { conn } = parseThen createP createCb
  where
    createCb args = do
      audit <- liftIO $ AuSt.create conn args
      let Audit { .. } = audit
      Just ballot <- liftIO $ ElSt.randomBallot conn auElectionId
      let Ballot { balId } = ballot
      liftIO $ AuSt.setCurrentSample conn auId balId
      liftIO (mkJson conn audit) >>= json

createP :: Object -> Parser CreateData
createP o = do
  elId      <- o .: "electionId"
  date      <- o .: "date"
  riskLimit <- o .: "riskLimit"
  contests  <- o .: "contests"
  return (elId, date, riskLimit, contests)

getById :: Controller
getById State { conn } = do
  auId <- param "id"
  audit <- liftIO $ AuSt.getById conn auId
  let ifJust a = liftIO (mkJson conn a) >>= json
  maybe (status notFound404) ifJust audit

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
  let ifJust a = liftIO (mkJson conn a) >>= json
  maybe (status notFound404) ifJust audit

setActive :: Controller
setActive State { conn } = parseThen (.: "auditId") setActiveCb
  where
    setActiveCb auId = liftIO (AuSt.setActive conn auId) >>= json

indexMarks :: Controller
indexMarks State { conn } = do
  auId <- param "id"
  marks <- liftIO $ do
    ungrouped <- AuSt.indexMarks conn auId
    let cmp = compare `on` amBallotId
        sorted = sortBy cmp ungrouped
        eq m m' = amBallotId m == amBallotId m'
        grouped = groupBy eq sorted
    forM grouped $ \grp -> do
      let balId = amBallotId (head grp)
      grpMarks <- forM grp $ \am -> do
        let AuditMark { .. } = am
        return [aesonQQ|{
          contestId: #{amContestId},
          candidateId: #{amCandidateId}
        }|]
      return [aesonQQ|{ ballotId: #{balId}, marks: #{grpMarks} }|]
  json marks

createMarks :: Controller
createMarks State { conn } = parseThen createMarksP createMarksCb
  where
    createMarksCb (amBallotId, markData) = do
      amAuditId <- param "id"
      audit <- liftIO $ AuSt.getById conn amAuditId >>= return . fromJust
      let Audit { auElectionId } = audit
          mkMarkJson (amContestId, amCandidateId) = do
            let auditMark = AuditMark { .. }
            liftIO $ AuSt.createMark conn auditMark
            return [aesonQQ|{
              contestId: #{amContestId},
              candidateId: #{amCandidateId}
            }|]
      marks <- liftIO $ forM markData mkMarkJson
      newBallot <- liftIO $ ElSt.randomBallot conn auElectionId >>= return . fromJust
      let Ballot { balId } = newBallot
      liftIO $ AuSt.setCurrentSample conn amAuditId balId
      json marks

createMarksP :: Object -> Parser (Integer, [(Integer, Integer)])
createMarksP o = do
  ballotId <- o .: "ballotId"
  marksArr <- o .: "marks"
  marks <- forM marksArr $ \m -> do
    contestId   <- m .: "contestId"
    candidateId <- m .: "candidateId"
    return (contestId, candidateId)
  return (ballotId, marks)

currentSample :: Controller
currentSample State { conn } = do
  auId <- param "id"
  sample <- liftIO $ do
    sampleId <- AuSt.currentSampleId conn auId
    BalSt.getById conn (fromJust sampleId)
  json sample
