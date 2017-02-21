import { fetch, submit } from '../util';

import fetchBallots from './fetchBallots';


export default filePaths => (dispatch, getState) => {
  const [filePath] = filePaths;
  const electionId = getState().election.id;
  const data = {
    electionId,
    vendor: 'dominion',
    type: 'ballot',
    filePath,
  };
  submit('/manifest', data)
    .then(() => fetchBallots(electionId)(dispatch));
}
