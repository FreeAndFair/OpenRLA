import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import isElectionDefined from '../selector/is-election-defined';


const ArchiveInfoCard = ({ archive, navigateArchive }) => {
  const navigateButton = (
    <button onClick={navigateArchive}>View archived audits</button>
  );

  let status;

  if (_.isEmpty(archive)) {
    status = <div>No archived audits.</div>;
  } else {
    status = <div>Archive audits exist.</div>;
  }

  return (
    <div>
      {status}
      {navigateButton}
    </div>
  );
};

ArchiveInfoCard.propTypes = {
  archive: PropTypes.object.isRequired,
  navigateArchive: PropTypes.func.isRequired,
};

const mapStateToProps = ({ archive }) => ({
  archive,
});

const mapDispatchToProps = dispatch => ({
  navigateArchive: () => dispatch({ type: 'NAVIGATE_ARCHIVE' }),
});

export default connect(mapStateToProps, mapDispatchToProps)(ArchiveInfoCard);
