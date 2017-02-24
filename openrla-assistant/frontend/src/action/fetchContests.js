import { fetch } from '../util';

import pivotContests from '../selector/pivotContests';


export default id => dispatch => {
  return fetch(`/election/${id}/contest`)
    .then(contests => dispatch({
      type: 'UPDATE_CONTESTS',
      contests: pivotContests(contests),
    }))
    .catch(console.error);
}
