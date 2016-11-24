module Vendor.Dominion where

import           Control.Monad (forM)
import           Data.Aeson (Object, (.:))
import           Data.Aeson.Types (Parser, parseMaybe)
import           Data.Maybe (fromJust)
import           Data.Text (Text)


processManifest :: Text -> Object -> IO ()
processManifest mType mObj = process mObj
  where
    process = case mType of
      "ballot"    -> processBallotManifest
      "candidate" -> processCandidateManifest
      "contest"   -> processContestManifest
      _           -> error "Invalid manifest type"

processBallotManifest :: Object -> IO ()
processBallotManifest = undefined

parseBallotManifest :: Object -> Parser ()
parseBallotManifest = undefined

processCandidateManifest :: Object -> IO ()
processCandidateManifest o = do
  let candidates = fromJust $ parseMaybe candidateManifestP o
  return ()

candidateManifestP :: Object -> Parser [(Integer, Text, Text, Text, Text)]
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

processContestManifest :: Object -> IO ()
processContestManifest o = do
  let contests = fromJust $ parseMaybe candidateManifestP o
  return ()

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
