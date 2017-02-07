import _ from 'lodash';


const pivotBallotMarks = ballotMarks => {
  const { ballotId, marks } = ballotMarks;

  return {
    ballotId,
    marks: _.keyBy(marks, m => m.contestId),
  };
};


export default auditMarks => {
  const pivotedBallotMarks = _.map(auditMarks, pivotBallotMarks);
  return _.keyBy(pivotBallotMarks, m => m.ballotId);
};
