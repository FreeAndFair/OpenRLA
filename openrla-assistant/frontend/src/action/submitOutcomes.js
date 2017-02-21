import { fetch, submit } from '../util';

import fetchOutcomes from './fetchOutcomes';


export default (id, outcomes) => dispatch => {
  submit(`/election/${id}/outcome`, outcomes)
    .then(() => {
      fetchOutcomes(id)(dispatch);
    });
};
