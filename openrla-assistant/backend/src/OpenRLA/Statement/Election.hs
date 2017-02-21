module OpenRLA.Statement.Election where

import           Control.Monad (when)
import           Data.Maybe (fromJust)
import           Data.String.Here (here)
import           Data.Text (Text)
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (Connection, Only(..))
import           System.Random (randomRIO)

import           OpenRLA.Statement
import qualified OpenRLA.Statement.Ballot as BalSt
import           OpenRLA.Types


create :: Connection -> Text -> Text -> IO Election
create conn elTitle elDate = do
  let s = "insert or replace into election (title, date) values (?, ?)"
  Sql.execute conn s (elTitle, elDate)
  rowId <- Sql.lastInsertRowId conn
  let elId = fromIntegral rowId
  setActive conn elId
  maybeElection <- getById conn elId
  return $ fromJust maybeElection

index :: Connection -> IO [Election]
index conn = Sql.query_ conn s
  where s = "select id, title, date, active from election order by date"

getActive :: Connection -> IO (Maybe Election)
getActive conn
  = Sql.query_ conn s >>= justOneIO
  where
    s = "select id, title, date, active from election where active"

setActive :: Connection -> Integer -> IO ()
setActive conn elId
  = Sql.withTransaction conn $ do
      resetActive conn
      Sql.execute  conn "update election set active = 1 where id = ?" (Only elId)

getById :: Connection -> Integer -> IO (Maybe Election)
getById conn elId
  = Sql.query conn s (Only elId) >>= justOneIO
  where s = "select id, title, date, active from election where id = ?"

setById :: Connection -> Election -> IO ()
setById conn election@Election { elId, elActive } = do
  let s = "insert or replace into election (id, title, date) values (?, ?, ?)"
  Sql.execute conn s election
  when elActive $ setActive conn elId

resetActive :: Connection -> IO ()
resetActive conn = Sql.execute_ conn "update election set active = null where active = 1"

ballotCountForId :: Connection -> Integer -> IO Integer
ballotCountForId conn elId = do
  let s = "select count() from election_ballot where election_id = ?"
  rows <- Sql.query conn s (Only elId)
  let Only count = oneRow rows
  return count

getBallots :: Connection -> Integer -> IO [Ballot]
getBallots conn elId = do
  let s = [here|
    select b.id, b.src_path, b.file_path
      from ballot b
      join election_ballot eb
        on b.id = eb.ballot
     where eb.election_id = ?
  |]
  Sql.query conn s (Only elId)

getContests :: Connection -> Integer -> IO [Contest]
getContests conn elId = do
  let s = [here|
    select c.id, c.external_id, c.description, c.num_ranks, c.vote_for
      from contest c
      join election_contests ec
        on c.id = ec.contest_id
     where ec.election_id = ?
  |]
  rows <- Sql.query conn s (Only elId)
  return rows

randomBallot :: Connection -> Integer -> IO (Maybe Ballot)
randomBallot conn elId = do
    count  <- ballotCountForId conn elId
    offset <- randomRIO (0, count - 1)
    BalSt.getByOffset conn offset

indexElectionOutcomes :: Connection -> Integer -> IO [ContestOutcome]
indexElectionOutcomes conn elId = Sql.query conn s (Only elId)
  where s = [here|
    select election_id, contest_id, candidate_id, share
      from election_contest_outcome
     where election_id = ?
  |]

getContestOutcomes :: Connection -> Integer -> Integer -> IO [ContestOutcome]
getContestOutcomes conn elId contId = Sql.query conn s (elId, contId)
  where s = [here|
    select election_id, contest_id, candidate_id, share
      from election_contest_outcome
     where election_id = ?
       and contest_id = ?
  |]

setContestOutcome :: Connection -> ContestOutcome -> IO ()
setContestOutcome conn outcome = Sql.execute conn s outcome
  where s = [here|
    insert or replace into election_contest_outcome (
      election_id,
      contest_id,
      candidate_id,
      share
    )
    values (?, ?, ?, ?)
  |]
