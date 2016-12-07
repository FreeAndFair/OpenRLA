module OpenRLA.Statement.Audit where

import           Database.SQLite.Simple (Connection)

import           OpenRLA.Types


getAuditById :: Connection -> Integer -> IO (Maybe Audit)
getAuditById = undefined

setAuditById  :: Connection -> Integer -> IO ()
setAuditById = undefined

getActiveAudit :: Connection -> IO (Maybe Audit)
getActiveAudit = undefined

setActiveAudit :: Connection -> Integer -> IO ()
setActiveAudit = undefined
