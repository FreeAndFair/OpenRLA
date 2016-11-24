module Controller.Audit where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.:))
import           Web.Scotty (json)

import           Controller
import qualified Query as Q
import           Types (State(..))


index :: Controller
index = undefined

create :: Controller
create = undefined

getById :: Controller
getById State { conn } = parseThen (.: "auditId") cb
  where
    cb auditId = liftIO (Q.getAuditById conn auditId) >>= json

setById :: Controller
setById = undefined

getActive :: Controller
getActive State { conn } = liftIO (Q.getActiveAudit conn) >>= json

setActive :: Controller
setActive State { conn } = parseThen (.: "auditId") cb
  where
    cb auditId = liftIO (Q.setActiveAudit conn auditId) >>= json
