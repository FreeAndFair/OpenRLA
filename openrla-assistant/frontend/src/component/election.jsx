import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import isElectionDefined from '../selector/is-election-defined';
import HomeButton from './home-button';


const Election = ({ election }) => {
  return (
    <div>
      <div>Election page.</div>
      <HomeButton />
  </div>);
};

Election.propTypes = {
  election: PropTypes.object.isRequired,
};

const mapStateToProps = ({ election }) => ({ election });

export default connect(mapStateToProps)(Election);
