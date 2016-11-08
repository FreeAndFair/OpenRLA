import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import isElectionDefined from '../selector/is-election-defined';


const ElectionInfoCard = ({election, electionDefined}) => {
  if (_.isEmpty(election)) {
    return <div>Election is not defined.</div>;
  }

  if (!electionDefined) {
    return <div>Election definition is incomplete.</div>;
  }

  return <div>Election is defined.</div>
};

ElectionInfoCard.propTypes = {
  election: PropTypes.object.isRequired,
  electionDefined: PropTypes.bool.isRequired,
};

const mapStateToProps = ({ election }) => ({
  election,
  electionDefined: isElectionDefined(election),
});

export default connect(mapStateToProps)(ElectionInfoCard);
