import { fetch, submit } from '../util';

import pivotContests from '../selector/pivot-contests';


export default filePaths => (dispatch, getState) => {
  const [filePath] = filePaths;
  const electionId = getState().election.id;
  const data = {
    electionId,
    vendor: 'dominion',
    type: 'contest',
    filePath,
  };
  submit('/manifest', data).then(() => {
    fetch(`/election/${electionId}/contest`)
      .then(contests => dispatch({
        type: 'UPDATE_CONTESTS',
        contests: pivotContests(contests),
      }));
  });
}
