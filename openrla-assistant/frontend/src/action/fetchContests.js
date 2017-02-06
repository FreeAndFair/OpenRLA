import { fetch } from '../util';

import pivotContests from '../selector/pivot-contests';


export default id => dispatch => {
  fetch(`/election/${id}/contest`)
    .then(contests => dispatch({
      type: 'UPDATE_CONTESTS',
      contests: pivotContests(contests),
    }))
    .catch(console.error);
}
