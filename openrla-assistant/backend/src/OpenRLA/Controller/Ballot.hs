module OpenRLA.Controller.Ballot where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson ((.:))
import           Web.Scotty (file, setHeader)

import           OpenRLA.Controller
import qualified OpenRLA.Statement as St
import           OpenRLA.Types (State(..))


index :: Controller
index = undefined

create :: Controller
create = undefined

getById :: Controller
getById State { conn } = parseThen (.: "ballotId") cb
  where
    cb ballotId = liftIO (St.getBallotPathById conn ballotId) >>= trySend

    trySend Nothing     = return ()
    trySend (Just path) = do
      setHeader "content-type" "image/png"
      file path

setById :: Controller
setById = undefined
