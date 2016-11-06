const defaultState = { count: 0 };

export default (state=defaultState, action) => {
  switch (action.type) {
  case 'INCREMENT':
    state.count += 1;
    break;
  case 'RESET':
    state.count = 0;
    break;
  }
  return state;
};
