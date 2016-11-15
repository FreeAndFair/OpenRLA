const coloradoElection = {
  title: '2016 Clear Creek Primary Election',
  date: '2016-07-13T12:00:00.000Z',
  contests: {
    1001: {
      description: 'United States Senator - DEM',
      externalId: '600013282',
      id: 1001,
      numRanks: 0,
      voteFor: 1,
    },
    1002: {
      id: 1002,
      externalId: '600013270',
      description: 'Representative to the 115th United States Congress - District 2 - DEM',
      numRanks: 0,
      voteFor: 1,
    },
    1009: {
      id: 1009,
      externalId: '600013283',
      description: 'United States Senator - REP',
      numRanks: 0,
      voteFor: 1,
    },
  },
  candidates: {
    1: {
      "contestId": 1001,
      "description": "Michael Bennet",
      "externalId": "1",
      "id": 1,
      "type": "Regular"
    },
    2: {
      "contestId": 1002,
      "description": "Jared Polis",
      "externalId": "1",
      "id": 2,
      "type": "Regular"
    },
    3: {
      "contestId": 1002,
      "description": "Write-in",
      "externalId": "",
      "id": 3,
      "type": "WriteIn"
    },

    10: {
      "contestId": 1009,
      "description": "Darryl Glenn",
      "externalId": "1",
      "id": 10,
      "type": "Regular"
    },
    11: {
      "contestId": 1009,
      "description": "Ryan L. Frazier",
      "externalId": "2",
      "id": 11,
      "type": "Regular"
    },
    12: {
      "contestId": 1009,
      "description": "Robert Blaha",
      "externalId": "3",
      "id": 12,
      "type": "Regular"
    },
    13: {
      "contestId": 1009,
      "description": "Jack Graham",
      "externalId": "4",
      "id": 13,
      "type": "Regular"
    },
    14: {
      "contestId": 1009,
      "description": "Jon Keyser",
      "externalId": "5",
      "id": 14,
      "type": "Regular"
    },
    15: {
      "contestId": 1009,
      "description": "Write-in",
      "externalId": "",
      "id": 15,
      "type": "WriteIn"
    },
  },
};

const noElection = {};

const makeArchivedAudit = () => ({
  election: coloradoElection,
  date: (new Date()).toISOString(),
});

const testArchive = [];


export default {
  page: 'home',
  election: coloradoElection,
  audit: {},
  archive: testArchive,
};
