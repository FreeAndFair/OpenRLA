import React from 'react';
import ReactDOM from 'react-dom';
import { createStore } from 'redux';
import { Provider } from 'react-redux';

import main from './reducer';
import Main from './component/main';


const store = createStore(main);

const tree = (
  <Provider store={store}>
    <Main />
  </Provider>
);

window.onload = () => {
  const mainEl = document.getElementById('main');
  ReactDOM.render(tree, mainEl);
};
