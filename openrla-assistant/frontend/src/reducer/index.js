import { merge } from 'lodash';

import defaultState from './default-state';


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
  default: { return state; }
  }
};
