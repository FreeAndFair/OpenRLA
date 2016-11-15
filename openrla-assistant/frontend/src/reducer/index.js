import defaultState from './default-state';


const merge = (...objects) => Object.assign({}, ...objects);


export default (state = defaultState, action) => {
  switch (action.type) {
  case 'SET_PAGE':
    const { page } = action;
    return merge(state, { page });
  default:
    return state;
  }
};
