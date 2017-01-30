module OpenRLA.Vendor.Dominion where

import           Control.Monad (forM, forM_)
import           Data.Aeson (Object, Value, (.:), (.=), object, toJSON)
import           Data.Aeson.Types (Parser, parseMaybe)
import           Data.Maybe (fromJust)
import           Data.Text (Text)

import qualified OpenRLA.Statement as St
import           OpenRLA.Types (State(..))


processManifest :: State -> Integer -> Text -> Object -> IO Value
processManifest state eId mType mObj = process state eId mObj
  where
    process = case mType of
      "candidate" -> processCandidateManifest
      "contest"   -> processContestManifest
      _           -> error "Invalid manifest type"

processCandidateManifest :: State -> Integer -> Object -> IO Value
processCandidateManifest (State { .. }) _eId o = do
  let candidates = fromJust $ parseMaybe candidateManifestP o
  forM_ candidates (St.upsertCandidate conn)
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

processContestManifest :: State -> Integer -> Object -> IO Value
processContestManifest (State {..}) eId o = do
  let contests = fromJust $ parseMaybe contestManifestP o
  forM_ contests $ (St.upsertContest conn eId)
  return $ toJSON (map toVal contests)
    where
      toVal (cId, extId, desc, nRanks, vFor)
        = object [ "id"         .= cId
                 , "externalId" .= extId
                 , "desc"       .= desc
                 , "numRanks"   .= nRanks
                 , "voteFor"    .= vFor
                 ]
  -- return $ object []

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
