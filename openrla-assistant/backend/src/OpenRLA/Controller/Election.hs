module OpenRLA.Controller.Election where

import           Control.Monad (forM, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Object, Value(..), (.:))
import           Data.Aeson.QQ (aesonQQ)
import           Data.Aeson.Types (Parser)
import           Data.Function (on)
import           Data.List (sortBy, groupBy)
import           Data.Maybe (fromJust, maybe)
import           Data.Text (Text)
import           Network.HTTP.Types.Status (notFound404)
import           Web.Scotty (json, param, status)

import           OpenRLA.Controller
import qualified OpenRLA.Statement as St
import qualified OpenRLA.Statement.Election as ElSt
import           OpenRLA.Types (
    Candidate(..)
  , Contest(..)
  , ContestOutcome(..)
  , Election(..)
  , State(..)
  )


index :: Controller
index State { conn } = liftIO (ElSt.index conn) >>= json

create :: Controller
create State { conn } = parseThen createP createCb
  where
    createCb (elTitle, elDate) = do
      election <- liftIO $ ElSt.create conn elTitle elDate
      json election

createP :: Object -> Parser (Text, Text)
createP o = do
  title <- o .: "title"
  date  <- o .: "date"
  return (title, date)

getById :: Controller
getById State { conn } = do
  elId <- param "id"
  res <- liftIO (ElSt.getById conn elId)
  maybe (status notFound404) json res

setById :: Controller
setById State { conn } = parseThen setByIdP setByIdCb
  where
    setByIdCb (elTitle, elDate, elActive) = do
      elId <- param "id"
      let election = Election { .. }
      liftIO $ ElSt.setById conn election
      json election

setByIdP :: Object -> Parser (Text, Text, Bool)
setByIdP o = do
  title  <- o .: "title"
  date   <- o .: "date"
  active <- o .: "active"
  return (title, date, active)

getActive :: Controller
getActive State { conn } = do
  active <- liftIO $ ElSt.getActive conn
  maybe (status notFound404) json active

setActive :: Controller
setActive State { conn } = parseThen (.: "electionId") cb
  where
    cb eId = liftIO (ElSt.setActive conn eId) >>= json

sampleBallot :: Controller
sampleBallot State { conn } = do
  elId  <- param "id"
  ballot <- liftIO $ ElSt.randomBallot conn elId
  json $ fromJust ballot

getBallotsById :: Controller
getBallotsById State { conn } = do
  elId  <- param "id"
  ballots <- liftIO $ ElSt.getBallots conn elId
  json ballots

getContestsById :: Controller
getContestsById State { conn } = do
  elId <- param "id"
  contests <- liftIO $ ElSt.getContests conn elId
  let contestCb Contest { .. } = do
        candidates <- St.getContestCandidates conn contId
        let cb Candidate { .. } = [aesonQQ|{
              description: #{candDescription},
              id: #{candId},
              externalId: #{candExternalId},
              contestId: #{candContestId},
              type: #{candType}
            }|]
            candidatesJson = map cb candidates
        return [aesonQQ|{
          description: #{contDescription},
          id: #{contId},
          externalId: #{contExternalId},
          voteFor: #{contVoteFor},
          numRanks: #{contNumRanks},
          candidates: #{candidatesJson}
        }|]
  result <- liftIO $ forM contests contestCb

  json result

indexElectionOutcomes :: Controller
indexElectionOutcomes State { conn } = do
  elId <- param "id"
  rows <- liftIO $ ElSt.indexElectionOutcomes conn elId
  json $ formatOutcomes rows

formatOutcomes :: [ContestOutcome] -> Value
formatOutcomes outcomes = [aesonQQ|#{formatted}|]
  where cmp = compare `on` coContestId
        sorted = sortBy cmp outcomes
        eq o o' = coContestId o == coContestId o'
        grouped = groupBy eq sorted
        formatted = map formatOutcomeGroup grouped

formatOutcomeGroup :: [ContestOutcome] -> Value
formatOutcomeGroup grp = [aesonQQ|{
    id: #{contestId},
    shares: #{shares}
  }|]
  where contestId = coContestId $ head grp
        fmtShare ContestOutcome { .. } = [aesonQQ|{
          id: #{coCandidateId},
          share: #{coShare}
        }|]
        shares = map fmtShare grp

setElectionOutcomes :: Controller
setElectionOutcomes State { conn } = do
  elId      <- param "id"
  parseThen (outcomeP elId) $ \outcomes -> do
    forM_ outcomes $ \o -> do
      liftIO $ ElSt.setContestOutcome conn o
    json $ formatOutcomeGroup outcomes

outcomeP :: Integer -> Object -> Parser [ContestOutcome]
outcomeP coElectionId o = do
  coContestId <- o .: "id"
  sharesArr   <- o .: "shares"
  forM sharesArr $ \sh -> do
    coCandidateId <- sh .: "id"
    coShare       <- sh .: "share"
    return $ ContestOutcome { .. }

getContestOutcome :: Controller
getContestOutcome State { conn } = do
  elId   <- param "id"
  contId <- param "contestId"
  outcomes <- liftIO $ ElSt.getContestOutcomes conn elId contId
  json $ formatOutcomeGroup outcomes
