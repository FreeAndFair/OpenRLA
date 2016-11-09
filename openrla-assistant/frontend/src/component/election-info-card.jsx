import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import isElectionDefined from '../selector/is-election-defined';


const ElectionInfoCard = ({election, electionDefined, navigateElection}) => {
  const navigateButton = (
    <button onClick={navigateElection}>Configure election</button>
  );

  let status;

  if (_.isEmpty(election)) {
    status = <div>Election is not defined.</div>;
  } else if (!electionDefined) {
    status = <div>Election definition is incomplete.</div>;
  } else {
    status = <div>Election is defined.</div>
  }

  return (
    <div>
      {status}
      {navigateButton}
    </div>
  );
};

ElectionInfoCard.propTypes = {
  election: PropTypes.object.isRequired,
  electionDefined: PropTypes.bool.isRequired,
  navigateElection: PropTypes.func.isRequired,
};

const mapStateToProps = ({ election, }) => ({
  election,
  electionDefined: isElectionDefined(election),
});

const mapDispatchToProps = dispatch => ({
  navigateElection: () => dispatch({ type: 'NAVIGATE_ELECTION' }),
});

export default connect(mapStateToProps, mapDispatchToProps)(ElectionInfoCard);
