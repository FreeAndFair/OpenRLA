import { fetch } from '../util';


export default id => dispatch => {
  fetch(`/election/${id}/outcome`)
    .then(outcomes => dispatch({
      type: 'UPDATE_OUTCOMES',
      outcomes,
    }))
    .catch(console.error);
}
