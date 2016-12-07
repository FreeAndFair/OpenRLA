module OpenRLA.Controller.Audit where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.:))
import           Web.Scotty (json)

import           OpenRLA.Controller
import qualified OpenRLA.Statement as St
import           OpenRLA.Types (State(..))


index :: Controller
index = undefined

create :: Controller
create = undefined

getById :: Controller
getById State { conn } = parseThen (.: "auditId") cb
  where
    cb auditId = liftIO (St.getAuditById conn auditId) >>= json

setById :: Controller
setById = undefined

getActive :: Controller
getActive State { conn } = liftIO (St.getActiveAudit conn) >>= json

setActive :: Controller
setActive State { conn } = parseThen (.: "auditId") cb
  where
    cb auditId = liftIO (St.setActiveAudit conn auditId) >>= json