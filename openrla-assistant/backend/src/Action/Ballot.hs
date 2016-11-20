{-# LANGUAGE OverloadedStrings #-}
module Action.Ballot where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.:))
import           Web.Scotty (file, setHeader)

import           Controller
import qualified Query as Q


index :: Controller
index = undefined

create :: Controller
create = undefined

getById :: Controller
getById conn = parseThen (.: "ballotId") cb
  where
    cb ballotId = liftIO (Q.getBallotPathById conn ballotId) >>= trySend

    trySend Nothing     = return ()
    trySend (Just path) = do
      setHeader "content-type" "image/png"
      file path

setById :: Controller
setById = undefined
