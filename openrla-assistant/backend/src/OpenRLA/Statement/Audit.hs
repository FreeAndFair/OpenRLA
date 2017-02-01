module OpenRLA.Statement.Audit where

import           Control.Monad (forM_)
import           Data.Maybe (fromJust)
import           Data.String.Here (here)
import           Data.Text (Text)
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (Connection, Only(..))

import           OpenRLA.Statement (justOneIO, oneRowIO)
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
      Sql.execute  conn "update audit set active = 1 where id = ?" (Only auId)

resetActive :: Connection -> IO ()
resetActive conn = Sql.execute_ conn "update audit set active = null where active = 1"

indexMarks :: Connection -> Integer -> IO [AuditMark]
indexMarks conn auId = Sql.query conn s (Only auId)
  where
    s = "select audit_id, ballot_id, contest_id, candidate_id from audit_mark where audit_id = ?"

createMark :: Connection -> AuditMark -> IO ()
createMark conn auditMark = Sql.execute conn s auditMark
  where
    s = "insert or replace into audit_mark (audit_id, ballot_id, contest_id, candidate_id) values (?, ?, ?, ?)"

currentSampleId :: Connection -> Integer -> IO Integer
currentSampleId conn auId = do
  let s = "select ballot_id from audit_current_sample where audit_id = ?"
  rows <- Sql.query conn s (Only auId)
  Only sampleId <- oneRowIO rows
  return sampleId

addContest :: Connection -> Integer -> Integer -> IO ()
addContest conn auId contestId = do
  let s = [here|
    insert into audit_contest
    values (?, ?, 1.0)
  |]
  Sql.execute conn s (auId, contestId)

getContestData :: Connection -> Integer -> IO [(Integer, Double)]
getContestData conn auId = do
  let s = [here|
    select contest_id, test_statistic
      from audit_contest
     where audit_id = ?
  |]
  Sql.query conn s (Only auId)
