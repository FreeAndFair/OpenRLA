module OpenRLA.Statement.Election where

import           Data.Text (Text)
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (Connection, Only(..))

import           OpenRLA.Statement
import           OpenRLA.Types


type ElectionRow = (Integer, Text, Text, Bool)

createElection :: Connection -> Text -> Text -> IO Integer
createElection conn elTitle elDate
  = Sql.withTransaction conn $ do
      Sql.execute conn s (elTitle, elDate)
      rowId <- Sql.lastInsertRowId conn
      return $ fromIntegral rowId
        where
          s = "insert or replace into election (title, date) values (?, ?)"

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

setActiveElection :: Connection -> Integer -> IO ()
setActiveElection conn elId
  = Sql.withTransaction conn $ do
      Sql.execute_ conn "update election set active = null where active = 1"
      Sql.execute  conn "update election set active = 1 where id = ?" (Only elId)

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
