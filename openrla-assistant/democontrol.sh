#!/bin/bash
# democontrol.sh: set up the Dominion example election from Clear Creek county,
# and an audit of the only two contested contests, and
# present the correct information for the first 18 ballots.
# Show audit progress for each ballot.
# Uses 'jq' program to pretty-print the json responses.

# Preparation:
#  clean out the database via
#     cd backend/; make clean; cd -
#  ./demo.sh all &

# Usage: ./democontrol.sh
# Then hit "Enter" as prompted for each ballot

# The audit_cvrs application can be used to look up ballots and display results for
# those contests.  E.g.
#   ./cvr.py 726 | egrep 'R.*t 2 - DEM|^Un.*REP'

# Location of json manifest files
CVRDIR=$PWD/backend/test/data/dominion/example

# Set up election
curl -s 'http://localhost:8080/election' --data-binary '{"date":"Tue Jun 28 2016 00:00:00 GMT-0400 (EDT)","edited":true,"title":"Clear Creek 2016 Primary"}' --compressed
echo

# contest manifest
curl -s 'http://localhost:8080/manifest' --data-binary '{"electionId":1,"vendor":"dominion","type":"contest","filePath":"'$CVRDIR'/ContestManifest.json"}' --compressed
echo

# candidate manifest
curl -s 'http://localhost:8080/manifest' --data-binary '{"electionId":1,"vendor":"dominion","type":"candidate","filePath":"'$CVRDIR'/CandidateManifest.json"}' --compressed
echo

# cvrs
curl -s 'http://localhost:8080/manifest' --data-binary '{"electionId":1,"vendor":"dominion","type":"ballot","filePath":"'$CVRDIR'/CvrExport.json"}' --compressed > /dev/null
echo

# Set outcomes for CD2-DEM and Senate-REP contests
# Though it's actually 100%
curl -s -X POST -H  "Host: localhost:8080" -d "{\"id\":1002,\"shares\":[{\"id\":2,\"share\":0.95},{\"id\":3,\"share\":0.05}]}" http://localhost:8080/election/1/outcome
echo

curl -s -X POST -H  "Host: localhost:8080" -d "{\"id\":1009,\"shares\":[{\"id\":10,\"share\":0.329850746269},{\"id\":11,\"share\":0.0537313432836},{\"id\":12,\"share\":0.182089552239},{\"id\":13,\"share\":0.253731343284},{\"id\":14,\"share\":0.180597014925},{\"id\":15,\"share\":0}]}" http://localhost:8080/election/1/outcome
echo

# Start audit of those two

curl -s 'http://localhost:8080/audit' --data-binary '{"electionId":1,"date":"Fri Mar 24 2017 01:01:54 GMT-0400 (EDT)","riskLimit":0.1,"contests":[1002,1009]}' --compressed
echo

# Define some status functions

astat() {
  curl -s http://localhost:8080/audit/1/sample | jq -c '{id: .id, sequence: .sampleId}'
  curl -s http://localhost:8080/audit/active | jq -c '.contests'
  echo
}
export -f astat

mark() {
       echo "Hit Enter to enter next ballot"; read   # pause before each entry - just hit Enter in terminal
       curl -s 'http://localhost:8080/audit/1/marks' --compressed --data-binary "$@"
       astat
}
export -f mark

# Audit ballot 726: polis and invalid
mark '{"ballotId":726,"marks":[{"contestId":1002,"candidateId":2},{"contestId":1009}]}'

# Audit ballot 820: invalid and graham {"ballotId":295,"marks":[{"contestId":1002,"candidateId":2},{"contestId":1009}]}
#  "srcPath": "C:\\NAS\\2016 Clear Creek Primary Election\\Results\\Tabulator00003\\Batch1013\\Images\\00003_01013_000020*.*",

mark '{"ballotId":820,"marks":[{"contestId":1002},{"contestId":1009,"candidateId":13}]}'

# Audit ballot 384: polis and invalid
mark '{"ballotId":384,"marks":[{"contestId":1002,"candidateId":2},{"contestId":1009}]}'
# 723 invalid and glenn 
mark '{"ballotId":723,"marks":[{"contestId":1002},{"contestId":1009,"candidateId":10}]}'
# 928 invalid and keyser
mark '{"ballotId":928,"marks":[{"contestId":1002},{"contestId":1009,"candidateId":14}]}'
# 295 polis and invalid
mark '{"ballotId":295,"marks":[{"contestId":1002,"candidateId":2},{"contestId":1009}]}'
# 10 polis and invalid
mark '{"ballotId":10,"marks":[{"contestId":1002,"candidateId":2},{"contestId":1009}]}'
# 580 invalid and blaha  
mark '{"ballotId":580,"marks":[{"contestId":1002},{"contestId":1009,"candidateId":12}]}'
# 1174 invalid and blaha
mark '{"ballotId":1174,"marks":[{"contestId":1002},{"contestId":1009,"candidateId":12}]}'
# 498 invalid and glenn
mark '{"ballotId":498,"marks":[{"contestId":1002},{"contestId":1009,"candidateId":10}]}'
# 767 graham
mark '{"ballotId":767,"marks":[{"contestId":1002},{"contestId":1009,"candidateId":13}]}'
# 1091 polis
mark '{"ballotId":1091,"marks":[{"contestId":1002,"candidateId":2},{"contestId":1009}]}'
# 1134 both invalid
mark '{"ballotId":1091,"marks":[{"contestId":1002},{"contestId":1009}]}'
# 1249 both invalid
mark '{"ballotId":1249,"marks":[{"contestId":1002},{"contestId":1009}]}'
# 643 both invalid
mark '{"ballotId":643,"marks":[{"contestId":1002},{"contestId":1009}]}'
# 252 blaha
mark '{"ballotId":252,"marks":[{"contestId":1002},{"contestId":1009,"candidateId":12}]}'
# 256 keyser
mark '{"ballotId":256,"marks":[{"contestId":1002},{"contestId":1009,"candidateId":14}]}'
# 678 polis
mark '{"ballotId":678,"marks":[{"contestId":1002,"candidateId":2},{"contestId":1009}]}'
