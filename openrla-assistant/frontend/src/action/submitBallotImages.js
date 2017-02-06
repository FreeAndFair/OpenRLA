import { fetch, submit } from '../util';

import pivotBallots from '../selector/pivot-ballots';


export default filePaths => (dispatch, getState) => {
  const electionId = getState().election.id;
  const data = { electionId, filePaths };
  submit('/ballot', data).then(() => {
    fetch(`/election/${electionId}/ballot`)
      .then(ballots => dispatch({
        type: 'UPDATE_BALLOTS',
        ballots: pivotBallots(ballots),
      }));
  });
}
