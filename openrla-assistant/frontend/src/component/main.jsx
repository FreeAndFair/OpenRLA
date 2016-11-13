import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import Archive from './archive';
import Home from './home';
import Election from './election';


const Main = ({ page }) => {
  const Page = {
    archive: Archive,
    election: Election,
    home: Home,
  }[page];

  return <Page />;
};

Main.PropTypes = {
  page: PropTypes.string.isRequired,
};

const mapStateToProps = ({ page }) => ({ page });

export default connect(mapStateToProps)(Main);
