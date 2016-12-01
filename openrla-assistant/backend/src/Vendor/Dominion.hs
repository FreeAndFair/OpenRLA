module Vendor.Dominion where

import           Control.Monad (forM, forM_)
import           Data.Aeson (Object, Value, (.:), (.=), object, toJSON)
import           Data.Aeson.Types (Parser, parseMaybe)
import           Data.Maybe (fromJust)
import           Data.Text (Text)

import qualified Query as Q
import           Types (State(..))


processManifest :: State -> Text -> Object -> IO Value
processManifest state mType mObj = process state mObj
  where
    process = case mType of
      "ballot"    -> processBallotManifest
      "candidate" -> processCandidateManifest
      "contest"   -> processContestManifest
      _           -> error "Invalid manifest type"

processBallotManifest :: State -> Object -> IO Value
processBallotManifest = undefined

parseBallotManifest :: Object -> Parser Value
parseBallotManifest = undefined

processCandidateManifest :: State -> Object -> IO Value
processCandidateManifest (State { .. }) o = do
  let candidates = fromJust $ parseMaybe candidateManifestP o
  forM_ candidates (Q.upsertCandidate conn)
  return $ toJSON (map toVal candidates)
    where
      toVal (cId, extId, cType, contId, desc)
        = object [ "id"          .= cId
                 , "externalId"  .= extId
                 , "type"        .= cType
                 , "contestId"   .= contId
                 , "description" .= desc
                 ]


candidateManifestP :: Object -> Parser [(Integer, Text, Text, Integer, Text)]
candidateManifestP o = do
  candidates <- o .: "List" :: Parser [Object]
  forM candidates parse
    where
      parse v = do
        candidateId   <- v .: "Id"
        externalId    <- v .: "ExternalId"
        candidateType <- v .: "Type"
        contestId     <- v .: "ContestId"
        description   <- v .: "Description"
        return ( candidateId
               , externalId
               , candidateType
               , contestId
               , description
               )

processContestManifest :: State -> Object -> IO Value
processContestManifest st o = do
  let contests = fromJust $ parseMaybe candidateManifestP o
  return $ object []

contestManifestP :: Object -> Parser [(Integer, Text, Text, Integer, Integer)]
contestManifestP o = do
  contests <- o .: "List" :: Parser [Object]
  forM contests parse
    where
      parse v = do
        contestId   <- v .: "Id"
        externalId  <- v .: "ExternalId"
        description <- v .: "Description"
        numRanks    <- v .: "NumOfRanks"
        voteFor     <- v .: "VoteFor"

        return ( contestId
               , externalId
               , description
               , numRanks
               , voteFor
               )
