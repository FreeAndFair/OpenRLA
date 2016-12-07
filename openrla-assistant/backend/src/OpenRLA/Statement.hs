{-# LANGUAGE ScopedTypeVariables #-}
module OpenRLA.Statement where

import           Data.Text (Text)
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (
    Connection
  , Only(..)
  )

import           OpenRLA.Types


justOne :: [a] -> Maybe a
justOne [x] = Just x
justOne _   = Nothing

justOneIO :: [a] -> IO (Maybe a)
justOneIO = return . justOne

getContestCandidates :: Connection -> Integer -> IO [Candidate]
getContestCandidates conn contestId
  = Sql.query conn s (Only contestId)
  where
    s = "select id, external_id, contest_id, description, type from election_contests where contest_id = ?"

getAuditById :: Connection -> Integer -> IO (Maybe Audit)
getAuditById = undefined

setAuditById  :: Connection -> Integer -> IO ()
setAuditById = undefined

getActiveAudit :: Connection -> IO (Maybe Audit)
getActiveAudit = undefined

setActiveAudit :: Connection -> Integer -> IO ()
setActiveAudit = undefined

createManifest :: Connection -> (Text, Text, Text) -> IO (Integer)
createManifest conn index
  = Sql.withTransaction conn $ do
      Sql.execute conn s index
      rowId <- Sql.lastInsertRowId conn
      return $ fromIntegral rowId
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
