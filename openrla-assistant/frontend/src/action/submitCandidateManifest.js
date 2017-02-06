import { fetch, submit } from '../util';

import pivotContests from '../selector/pivot-contests';


export default filePath => (dispatch, getState) => {
  const electionId = getState().election.id;
  const data = {
    electionId,
    vendor: 'dominion',
    type: 'candidate',
    filePath,
  };
  submit('/manifest', data)
    .then(manifest => {
      dispatch({
        type: 'UPDATE_CANDIDATE_MANIFEST',
        manifest,
      });
      fetch(`/election/${electionId}/contest`)
        .then(contests => dispatch({
          type: 'UPDATE_CANDIDATES',
          contests: pivotContests(contests),
        }));
    });
}
