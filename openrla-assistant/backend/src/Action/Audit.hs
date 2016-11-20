{-# LANGUAGE OverloadedStrings #-}
module Action.Audit where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.:))
import           Web.Scotty (json)

import           Controller
import qualified Query as Q


index :: Controller
index = undefined

create :: Controller
create = undefined

getById :: Controller
getById conn = parseThen (.: "auditId") cb
  where
    cb auditId = liftIO (Q.getAuditById conn auditId) >>= json

setById :: Controller
setById = undefined

getActive :: Controller
getActive conn = liftIO (Q.getActiveAudit conn) >>= json

setActive :: Controller
setActive conn = parseThen (.: "auditId") cb
  where
    cb auditId = liftIO (Q.setActiveAudit conn auditId) >>= json
