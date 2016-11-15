import React from 'react';
import ReactDOM from 'react-dom';
import { createStore } from 'redux';
import { Provider } from 'react-redux';

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import getMuiTheme from 'material-ui/styles/getMuiTheme';

import injectTapEventPlugin from 'react-tap-event-plugin';

import mainReducer from './reducer';
import Main from './component/main';


// See: http://www.material-ui.com/#/customization/themes#api
const theme = getMuiTheme({
  palette: {
    primary1Color: '#09535f',
    primary2Color: '#063a42',
    accent1Color: '#ff7f00',
    pickerHeaderColor: '#09535f',
  },
});

const store = createStore(mainReducer);

const tree = (
  <MuiThemeProvider muiTheme={theme}>
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
