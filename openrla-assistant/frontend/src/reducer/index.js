const defaultState = {
  page: 'home',
  election: {},
  audit: {},
  archive: {},
};

export default (state = defaultState, action) => {
  switch (action.type) {
  case 'SET_PAGE':
    const { page } = action;
    return Object.assign({}, state, { page });
  default:
    return state;
  }
};
