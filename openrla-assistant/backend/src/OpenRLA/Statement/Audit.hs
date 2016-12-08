module OpenRLA.Statement.Audit where

import           Data.Text (Text)
import           Database.SQLite.Simple as Sql
import           Database.SQLite.Simple (Connection)

import           OpenRLA.Types


create :: Connection -> (Integer, Text, Double) -> IO Integer
create conn args = do
  let s = "insert into audit (election_id, date, risk_limit) values (?, ?, ?)"
  Sql.execute conn s args
  rowId <- Sql.lastInsertRowId conn
  return $ fromIntegral rowId

getAuditById :: Connection -> Integer -> IO (Maybe Audit)
getAuditById = undefined

setAuditById  :: Connection -> Integer -> IO ()
setAuditById = undefined

getActiveAudit :: Connection -> IO (Maybe Audit)
getActiveAudit = undefined

setActiveAudit :: Connection -> Integer -> IO ()
setActiveAudit = undefined
