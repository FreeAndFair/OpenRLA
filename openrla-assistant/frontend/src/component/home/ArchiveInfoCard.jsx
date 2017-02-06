import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardActions,
  CardTitle,
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';

import _ from 'lodash';

import setPage from '../../action/setPage';
import isElectionDefined from '../../selector/isElectionDefined';


const ArchiveInfoCard = ({ archive, navigateArchive }) => {
  let status;

  if (_.isEmpty(archive)) {
    status = <div>No archived audits.</div>;
  } else {
    status = <div>Archive audits exist.</div>;
  }

  return (
    <Card>
      <CardTitle
         title="Archive"
         subtitle="View or restart archived audits" />
      <CardActions>
        <RaisedButton label="View" onClick={navigateArchive} />
      </CardActions>
    </Card>
  );
};

ArchiveInfoCard.propTypes = {
  archive: PropTypes.array.isRequired,
  navigateArchive: PropTypes.func.isRequired,
};

const mapStateToProps = ({ archive }) => ({
  archive,
});

const mapDispatchToProps = dispatch => ({
  navigateArchive: () => dispatch(setPage('archive')),
});

export default connect(mapStateToProps, mapDispatchToProps)(ArchiveInfoCard);
