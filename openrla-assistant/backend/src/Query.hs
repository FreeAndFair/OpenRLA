{-# LANGUAGE ScopedTypeVariables #-}
module Query (
    createManifest
  , setManifestPathForId
  , getElectionIndex
  , getActiveElection
  , setActiveElection
  , getElectionById
  , getBallotPathById
  , getAuditById
  , setAuditById
  , getActiveAudit
  , setActiveAudit
  , upsertCandidate
  , upsertContest
  ) where

import           Control.Monad (forM)
import           Data.Text (Text)
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (
    Connection
  , Only(..)
  )

import Types


justOne :: [a] -> Maybe a
justOne [x] = Just x
justOne _   = Nothing

justOneIO :: [a] -> IO (Maybe a)
justOneIO = return . justOne

type ElectionRow = (Integer, Text, Text, Bool)

getElectionIndex :: Connection -> Integer -> Integer -> IO [ElectionRow]
getElectionIndex conn offset limit
  = Sql.query conn s (offset, limit)
  where
    s = "select id, title, date, active from election order by date limit ? offset ?"

getActiveElection :: Connection -> IO (Maybe Election)
getActiveElection conn
  = Sql.query_ conn s >>= justOneIO
  where
    s = "select id, title, date, active from election where active"

getElectionById :: Connection -> Integer -> IO (Maybe Election)
getElectionById conn elId
  = Sql.query conn s (Only elId) >>= justOneIO
  where
    s = "select id, title, date, active from election where active where id = ?"

getElectionContests :: Connection -> Integer -> IO [Contest]
getElectionContests conn elId
  = Sql.query conn s (Only elId)
  where
    s = "select contest_id from election_contests where election_id = ?"

getContestCandidates :: Connection -> Integer -> IO [Candidate]
getContestCandidates conn contestId
  = Sql.query conn s (Only contestId)
  where
    s = "select id, external_id, contest_id, description, type from election_contests where contest_id = ?"

setActiveElection :: Connection -> Integer -> IO ()
setActiveElection conn elId
  = Sql.withTransaction conn $ do
      Sql.execute_ conn "update election set active = null where active = 1"
      Sql.execute  conn "update election set active = 1 where id = ?" (Only elId)

getBallotPathById :: Connection -> Integer -> IO (Maybe FilePath)
getBallotPathById conn ballotId = undefined

getAuditById :: Connection -> Integer -> IO (Maybe Audit)
getAuditById = undefined

setAuditById  :: Connection -> Integer -> IO ()
setAuditById = undefined

getActiveAudit :: Connection -> IO (Maybe Audit)
getActiveAudit = undefined

setActiveAudit :: Connection -> Integer -> IO ()
setActiveAudit = undefined

createManifest :: Connection -> (Text, Text, Text) -> IO ()
createManifest conn index
  = Sql.execute conn s index
  where
    s = "insert into manifest (vendor, type, src_path) values (?, ?, ?)"

setManifestPathForId :: Connection -> Integer -> FilePath -> IO ()
setManifestPathForId conn mId filePath
  = Sql.execute conn s (filePath, mId)
  where
    s = "update manifest set file_path = ? where id = ?"

type CandidateRow = (Integer, Text, Text, Integer, Text)

upsertCandidate :: Connection -> CandidateRow -> IO ()
upsertCandidate conn row
  = Sql.execute conn s row
  where
    s = "insert or replace into candidate (id, external_id, type, contest_id, description) values (?, ?, ?, ?, ?)"


type ContestRow = (Integer, Text, Text, Integer, Integer)

upsertContest :: Connection -> Integer -> ContestRow -> IO ()
upsertContest conn eId row
  = Sql.withTransaction conn $ do
      Sql.execute conn sUpsert row
      Sql.execute conn sLinkToElection (eId, cId)
        where
          (cId, _, _, _, _) = row
          sUpsert = "insert or replace into contest (id, external_id, description, num_ranks, vote_for) values (?, ?, ?, ?, ?)"
          sLinkToElection = "insert or replace into election_contests (election_id, contest_id) values (?, ?)"
