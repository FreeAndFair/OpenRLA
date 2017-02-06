import { fetch } from '../util';


export default () => dispatch => {
  fetch('/audit/active')
    .then(audit => dispatch({
      type: 'UPDATE_AUDIT',
      audit,
    }))
    .catch(console.error);
}
