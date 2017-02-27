module OpenRLA.Audit where

import           Data.Function (on)
import           Data.List (maximumBy, partition)
import           Data.Maybe (isJust)

import           OpenRLA.Types


computeRisk :: [ContestOutcome] -> [AuditMark] -> Double
computeRisk outcomes marks = (2*sW)^mW * (2 - 2*sW)^mL
  where winner = maximumBy (compare `on` coShare ) outcomes
        winnerId = coCandidateId winner
        sW = coShare winner
        voteForWinner AuditMark { amCandidateId }
          = maybe False (== winnerId) amCandidateId
        (winnerVotes, loserOrInvalidVotes) = partition voteForWinner marks
        isValid AuditMark { amCandidateId } = isJust amCandidateId
        loserVotes = filter isValid loserOrInvalidVotes
        mW = length winnerVotes
        mL = length loserVotes
