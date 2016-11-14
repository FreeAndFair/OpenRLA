import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import isElectionDefined from '../selector/is-election-defined';


const Election = ({ election }) => {
  return (
    <div>
      <h1>Election page</h1>
      <div>Election data</div>
      <div>Ballot Manifest</div>
      <div>Cast Vote Records</div>
      <div>Ballot Images</div>
      <div>RLA Inputs</div>
    </div>
  );
};

Election.propTypes = {
  election: PropTypes.object.isRequired,
};

const mapStateToProps = ({ election }) => ({ election });

export default connect(mapStateToProps)(Election);
