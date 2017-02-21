import { forEach, merge } from 'lodash';

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
  case 'UPDATE_AUDIT_MARKS': {
    const { marks } = action;
    return update(state, { audit: { marks } });
  }
  case 'UPDATE_AUDIT_SAMPLE': {
    const { sample } = action;
    return update(state, { audit: { sample } });
  }
  case 'UPDATE_OUTCOMES': {
    const { outcomes } = action;
    const newState = merge({}, state);

    forEach(outcomes, o => {
      const { id: contestId, shares } = o;

      forEach(shares, s => {
        const contest = newState.election.contests[contestId];
        if (!contest) return;
        if (!contest.candidates) return;

        contest.candidates[s.id].share = s.share;
      });
    });

    return update(state, newState);
  }
  default: { return state; }
  }
};
