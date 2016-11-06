import React from 'react';
import ReactDOM from 'react-dom';
import { createStore } from 'redux';

import Main from './component/main';
import main from './reducer';


const store = createStore(main);

window.onload = () => {
  const mainEl = document.getElementById('main');
  const render = () => ReactDOM.render(<Main store={store}/>, mainEl);
  render();
  store.subscribe(render);
};
