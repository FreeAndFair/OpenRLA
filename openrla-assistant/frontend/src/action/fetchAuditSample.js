import { fetch } from '../util';


export default id => dispatch => {
  fetch(`/audit/${id}/sample`)
    .then(sample => dispatch({
      type: 'UPDATE_AUDIT_SAMPLE',
      sample,
    }))
    .catch(console.error);
}
