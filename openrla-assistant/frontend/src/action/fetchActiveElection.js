import { fetch } from '../util';

export default () => dispatch => {
  fetch('/election/active')
    .then(election => dispatch({
      type: 'UPDATE_ELECTION',
      election,
    }))
    .catch(err => console.error)
  ;
}
