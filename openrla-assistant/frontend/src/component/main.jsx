import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import Start from './start';


const Main = ({ page }) => {
  const Page = {
    start: Start,
  }[page];

  return <Page />;
};

Main.PropTypes = {
  page: PropTypes.string.isRequired,
};

const mapStateToProps = ({ page }) => ({ page });

export default connect(mapStateToProps)(Main);
