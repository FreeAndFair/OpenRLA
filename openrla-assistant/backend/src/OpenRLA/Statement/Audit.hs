module OpenRLA.Statement.Audit where

import           Control.Monad (forM, forM_)
import           Data.Maybe (fromJust)
import           Data.String.Here (here)
import           Data.Text (Text)
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (Connection, Only(..))

import           OpenRLA.Audit (computeRisk)
import           OpenRLA.Statement (justOneIO)
import qualified OpenRLA.Statement.Election as ElSt
import           OpenRLA.Types


index :: Connection -> IO [Audit]
index conn = Sql.query_ conn s
  where s = "select id, election_id, date, risk_limit from audit order by date"

create :: Connection -> (Integer, Text, Double, [Integer]) -> IO Audit
create conn (elId, date, riskLimit, contestIds) = do
  let s = "insert into audit (election_id, date, risk_limit) values (?, ?, ?)"
  Sql.execute conn s (elId, date, riskLimit)
  rowId <- Sql.lastInsertRowId conn
  let auId = fromIntegral rowId
  setActive conn auId
  audit <- getById conn auId
  forM_ contestIds $ \cId -> do
    addContest conn auId cId
  return $ fromJust audit

getById :: Connection -> Integer -> IO (Maybe Audit)
getById conn auId = Sql.query conn s (Only auId) >>= justOneIO
  where s = "select id, election_id, date, risk_limit from audit where id = ?"

setById  :: Connection -> Audit -> IO ()
setById conn audit = Sql.execute conn s audit
  where
    s = "insert or update into audit (id, election_id, date, risk_limit) values (?, ?, ?, ?)"

getActive :: Connection -> IO (Maybe Audit)
getActive conn = Sql.query_ conn s >>= justOneIO
  where
    s = "select id, election_id, date, risk_limit from audit where active"

setActive :: Connection -> Integer -> IO ()
setActive conn auId
  = Sql.withTransaction conn $ do
      resetActive conn
      Sql.execute conn "update audit set active = 1 where id = ?" (Only auId)

resetActive :: Connection -> IO ()
resetActive conn = Sql.execute_ conn "update audit set active = null where active = 1"

createSample :: Connection -> Integer -> Integer -> IO Integer
createSample conn auId balId = do
  let s = [here|
        insert into audit_sample (audit_id, ballot_id)
        values (?, ?)
      |]
  Sql.execute conn s (auId, balId)
  rowId <- Sql.lastInsertRowId conn
  return $ fromIntegral rowId

createMark :: Connection -> AuditMark -> IO ()
createMark conn auditMark = do
  let s = [here|
        insert or replace
        into audit_mark (audit_sample_id, contest_id, candidate_id)
        values (?, ?, ?)
      |]
      AuditMark { .. } = auditMark
  Sql.execute conn s (amSampleId, amContestId, amCandidateId)

setCurrentSample :: Connection -> AuditSample -> IO ()
setCurrentSample conn sample = do
  let AuditSample { .. } = sample
      s = [here|
        insert or replace
        into audit_current_sample (audit_id, sample_id)
        values (?, ?)
      |]
  Sql.execute conn s (ausAuditId, ausId)

currentSampleId :: Connection -> Integer -> IO (Maybe Integer)
currentSampleId conn auId = do
  let s = [here|
        select aus.id
        from audit_current_sample acs
        join audit_sample aus
          on acs.sample_id = aus.id
        where acs.audit_id = ?
      |]
  rows <- Sql.query conn s (Only auId)
  return $ case rows of
    []              -> Nothing
    [Only sampleId] -> sampleId

currentSample :: Connection -> Integer -> IO (Maybe AuditSample)
currentSample conn auId = do
  let s = [here|
        select id, audit_id, ballot_id
        from audit_sample
        where id = ?
      |]
  maybeSampleId <- currentSampleId conn auId
  case maybeSampleId of
    Nothing -> return Nothing
    Just sampleId -> do
      rows <- Sql.query conn s (Only sampleId)
      justOneIO rows

addContest :: Connection -> Integer -> Integer -> IO ()
addContest conn auId contestId = do
  let s = [here|
    insert into audit_contest
    values (?, ?)
  |]
  Sql.execute conn s (auId, contestId)

getContestData :: Connection -> Integer -> IO [(Integer, Double)]
getContestData conn auId = do
  let s = [here|
    select contest_id
      from audit_contest
     where audit_id = ?
  |]
  rows <- Sql.query conn s (Only auId)
  forM rows $ \(Only contId) -> do
    Just audit <- getById conn auId
    let Audit { auElectionId } = audit
    outcomes <- ElSt.getContestOutcomes conn auElectionId contId
    marks <- indexContestMarks conn auId contId
    let risk = computeRisk outcomes marks
    return (contId, risk)

indexContestMarks :: Connection -> Integer -> Integer -> IO [AuditMark]
indexContestMarks conn auId contId = Sql.query conn s (auId, contId)
  where s = [here|
    select aus.audit_id,
           aus.ballot_id,
           aum.contest_id,
           aum.candidate_id,
           aus.id
      from audit_mark aum
      join audit_sample aus
        on aum.audit_sample_id = aus.id
     where aus.audit_id = ?
       and aum.contest_id = ?
  |]

indexMarks :: Connection -> Integer -> IO [AuditMark]
indexMarks conn auId = Sql.query conn s (Only auId)
  where s = [here|
      select aus.audit_id,
             aus.ballot_id,
             aum.contest_id,
             aum.candidate_id,
             aus.id
        from audit_mark aum
        join audit_sample aus
          on aum.audit_sample_id = aus.id
       where aus.audit_id = ?
    order by aus.id asc
  |]
