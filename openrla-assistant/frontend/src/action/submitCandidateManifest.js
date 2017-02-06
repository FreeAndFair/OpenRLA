import { fetch, submit } from '../util';

import pivotContests from '../selector/pivotContests';


export default filePaths => (dispatch, getState) => {
  const [filePath] = filePaths;
  const electionId = getState().election.id;
  const data = {
    electionId,
    vendor: 'dominion',
    type: 'candidate',
    filePath,
  };
  submit('/manifest', data).then(() => {
    fetch(`/election/${electionId}/contest`)
      .then(contests => dispatch({
        type: 'UPDATE_CANDIDATES',
        contests: pivotContests(contests),
      }));
  });
}
