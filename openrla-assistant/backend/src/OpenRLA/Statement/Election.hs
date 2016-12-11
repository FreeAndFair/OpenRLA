module OpenRLA.Statement.Election where

import           Data.Text (Text)
import qualified Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (Connection, Only(..))

import           OpenRLA.Statement
import           OpenRLA.Types


type ElectionRow = (Integer, Text, Text, Bool)

create :: Connection -> Text -> Text -> IO Integer
create conn elTitle elDate
  = Sql.withTransaction conn $ do
      Sql.execute conn s (elTitle, elDate)
      rowId <- Sql.lastInsertRowId conn
      return $ fromIntegral rowId
        where
          s = "insert or replace into election (title, date) values (?, ?)"

index :: Connection -> Integer -> Integer -> IO [ElectionRow]
index conn offset limit
  = Sql.query conn s (offset, limit)
  where
    s = "select id, title, date, active from election order by date limit ? offset ?"

getActive :: Connection -> IO (Maybe Election)
getActive conn
  = Sql.query_ conn s >>= justOneIO
  where
    s = "select id, title, date, active from election where active"

setActive :: Connection -> Integer -> IO ()
setActive conn elId
  = Sql.withTransaction conn $ do
      Sql.execute_ conn "update election set active = null where active = 1"
      Sql.execute  conn "update election set active = 1 where id = ?" (Only elId)

getById :: Connection -> Integer -> IO (Maybe Election)
getById conn elId
  = Sql.query conn s (Only elId) >>= justOneIO
  where
    s = "select id, title, date, active from election where active where id = ?"

setById :: Connection -> Election -> IO ()
setById conn election = Sql.execute conn s election
  where
    s = "insert or replace into election (id, title, date, active) values (?, ?, ?, ?)"
