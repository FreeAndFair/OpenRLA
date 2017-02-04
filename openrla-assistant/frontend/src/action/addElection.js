import { submit } from '../util';


export default formData => dispatch => {
  submit('/election', formData)
    .then(election => dispatch({
      type: 'UPDATE_ELECTION',
      election,
    }));
}
