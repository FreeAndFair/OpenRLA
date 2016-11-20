{-# LANGUAGE OverloadedStrings #-}
module Action.Election where

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
getById conn = parseThen (.: "electionId") cb
  where
    cb electionId = liftIO (Q.getElectionById conn electionId) >>= json

setById :: Controller
setById = undefined

getActive :: Controller
getActive conn = liftIO (Q.getActiveElection conn) >>= json

setActive :: Controller
setActive conn = parseThen (.: "electionId") cb
  where
    cb electionId = liftIO (Q.setActiveElection conn electionId) >>= json
