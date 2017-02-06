import { merge } from 'lodash';

import defaultState from './default-state';


const update = (...objects) => merge({}, ...objects);


export default (state = defaultState, action) => {
  switch (action.type) {
  case 'SET_PAGE':
    const { page } = action;
    return update(state, { page });
  case 'UPDATE_ELECTION':
    const { election } = action;
    return update(state, { election });
  case 'UPDATE_CANDIDATE_MANIFEST':
    return update(state, { manifest: { candidate: { uploaded: true } } });
  case 'UPDATE_CANDIDATES':
    return update(state, { election: { contests: action.contests }});
  case 'UPDATE_CONTEST_MANIFEST':
    return update(state, { manifest: { contest: { uploaded: true } } });
  case 'UPDATE_CONTESTS':
    const contests = action.contests;
    return update(state, { election: { contests } });
  default:
    return state;
  }
};
