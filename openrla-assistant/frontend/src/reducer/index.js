const defaultState = {
  page: 'home',
  election: {},
  audit: {},
  archive: {},
};


export default (state = defaultState, action) => {
  switch (action.type) {
  case 'NAVIGATE_ELECTION':
    return Object.assign({}, state, { page: 'election' });
  default:
    return state;
  }
};
