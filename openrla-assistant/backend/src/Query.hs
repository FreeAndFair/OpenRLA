{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Query (
    getActiveElection
  , setActiveElection
  , getElectionById
  , getBallotPathById
  , getAuditById
  , setAuditById
  , getActiveAudit
  , setActiveAudit
  ) where

import           Control.Monad (forM)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (
    Connection
  , Only(..)
  )

import Types


justOne :: [a] -> Maybe a
justOne [x] = Just x
justOne _   = Nothing

getActiveElection :: Connection -> IO (Maybe Election)
getActiveElection conn = do
  result <- getActiveElectionId conn
  case result of
    Nothing     -> return Nothing
    Just elecId -> getElectionById conn elecId

getElectionById :: Connection -> Integer -> IO (Maybe Election)
getElectionById conn elecId = do
  row <- getElectionRowById conn elecId
  case row of
    Nothing -> return Nothing
    Just (_, elecTitle, elecDate) -> do
      elecContests <- getElectionContests conn elecId
      let byContest Contest { contId } = getContestCandidates conn contId
      candidatesByContest <- forM elecContests byContest
      let elecCandidates = concat candidatesByContest
      return $ Just $ Election { .. }

getElectionRowById :: Connection -> Integer -> IO (Maybe (Integer, T.Text, T.Text))
getElectionRowById conn elecId = do
  let s = "select id, title, date from election where active where id = ?"
  rows <- Sql.query conn s (Only elecId)
  return $ justOne rows

getActiveElectionId :: Connection -> IO (Maybe Integer)
getActiveElectionId conn = do
  rows <- Sql.query_ conn "select id from election where active"
  return $ fmap fromOnly (justOne rows)

getElectionContests :: Connection -> Integer -> IO [Contest]
getElectionContests conn electionId
  = Sql.query conn q (Only electionId)
  where q = "select contest_id from election_contests where election_id = ?"

getContestCandidates :: Connection -> Integer -> IO [Candidate]
getContestCandidates conn contestId
  = Sql.query conn q [contestId]
  where q = "select id, external_id, contest_id, description, type from election_contests where contest_id = ?"

setActiveElection :: Connection -> Integer -> IO ()
setActiveElection conn electionId
  = Sql.withTransaction conn $ do
      Sql.execute_ conn "update election set active = null where active = 1"
      Sql.execute  conn "update election set active = 1 where id = ?" (Only electionId)

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
