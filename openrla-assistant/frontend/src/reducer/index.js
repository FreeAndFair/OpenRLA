const defaultState = { count: 0 };

export default (state = defaultState, action) => {
  switch (action.type) {
  case 'INCREMENT':
    return Object.assign({}, state, { count: state.count + 1 });
  case 'RESET':
    return Object.assign({}, state, { count: 0 });
  default:
    return state;
  }
};
