module OpenRLA.Statement.Election where

import           Control.Monad (when)
import           Data.Text (Text)
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (Connection, Only(..))

import           OpenRLA.Statement
import           OpenRLA.Types


create :: Connection -> Text -> Text -> IO Integer
create conn elTitle elDate
  = Sql.withTransaction conn $ do
      Sql.execute conn s (elTitle, elDate)
      rowId <- Sql.lastInsertRowId conn
      return $ fromIntegral rowId
        where
          s = "insert or replace into election (title, date) values (?, ?)"

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
  where
    s = "select id, title, date, active from election where active where id = ?"

setById :: Connection -> Election -> IO ()
setById conn election@Election { elId, elActive } = do
  Sql.withTransaction conn $ do
    let s = "insert or replace into election (id, title, date) values (?, ?, ?)"
    Sql.execute conn s election
    when elActive $ setActive conn elId

resetActive :: Connection -> IO ()
resetActive conn = Sql.execute_ conn "update election set active = null where active = 1"

ballotCountForId :: Connection -> Integer -> IO Integer
ballotCountForId conn elId = do
  let s = "select count() from election_ballot_image where election_id = ?"
  rows <- Sql.query conn s (Only elId)
  let Only count = oneRow rows
  return count
