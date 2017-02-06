import { fetch } from '../util';

import fetchBallots from './fetchBallots';
import fetchContests from './fetchContests';


export default () => dispatch => {
  fetch('/election/active')
    .then(election => dispatch({
      type: 'UPDATE_ELECTION',
      election,
    }))
    .then(({ election: { id } }) => {
      fetchBallots(id)(dispatch);
      fetchContests(id)(dispatch);
    })
    .catch(console.error);
}
