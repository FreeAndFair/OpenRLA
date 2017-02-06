import { merge } from 'lodash';

import defaultState from './defaultState';


const update = (...objects) => merge({}, ...objects);


export default (state = defaultState, action) => {
  switch (action.type) {
  case 'SET_PAGE': {
    const { page } = action;
    return update(state, { page });
  }
  case 'UPDATE_ELECTION': {
    const { election } = action;
    return update(state, { election });
  }
  case 'UPDATE_CANDIDATES': {
    const { contests } = action;
    return update(state, { election: { contests } });
  }
  case 'UPDATE_CONTESTS': {
    const { contests } = action;
    return update(state, { election: { contests } });
  }
  case 'UPDATE_BALLOTS': {
    const { ballots } = action;
    return update(state, { election: { ballots } });
  }
  case 'UPDATE_AUDIT': {
    const { audit } = action;
    return update(state, { audit });
  }
  default: { return state; }
  }
};
