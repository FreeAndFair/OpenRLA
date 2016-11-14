import React from 'react';
import ReactDOM from 'react-dom';
import { createStore } from 'redux';
import { Provider } from 'react-redux';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import injectTapEventPlugin from 'react-tap-event-plugin';

import mainReducer from './reducer';
import Main from './component/main';


const store = createStore(mainReducer);

const tree = (
  <MuiThemeProvider>
    <Provider store={store}>
      <Main />
    </Provider>
  </MuiThemeProvider>
);

window.onload = () => {
  injectTapEventPlugin();
  const mainEl = document.getElementById('main');
  ReactDOM.render(tree, mainEl);
};
