import { fetch } from '../util';

import pivotBallots from '../selector/pivot-ballots';


export default id => dispatch => {
  fetch(`/election/${id}/ballot`)
    .then(ballots => dispatch({
      type: 'UPDATE_BALLOTS',
      ballots: pivotBallots(ballots),
    }))
    .catch(console.error);
}
