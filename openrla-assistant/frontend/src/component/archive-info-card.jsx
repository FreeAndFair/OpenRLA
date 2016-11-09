import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import isElectionDefined from '../selector/is-election-defined';


const ArchiveInfoCard = ({ archive }) => {
  if (_.isEmpty(archive)) {
    return <div>No archive audits.</div>;
  }

  return <div>Archive audits exist.</div>;
};

ArchiveInfoCard.propTypes = {
  archive: PropTypes.object.isRequired,
};

const mapStateToProps = ({ archive }) => ({
  archive,
});

export default connect(mapStateToProps)(ArchiveInfoCard);
