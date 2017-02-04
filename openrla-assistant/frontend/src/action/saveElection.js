import { save } from '../util';


export default election => dispatch => {
  save(`/election/${election.id}`, election)
    .then(election => dispatch({
      type: 'UPDATE_ELECTION',
      election,
    }));
}
