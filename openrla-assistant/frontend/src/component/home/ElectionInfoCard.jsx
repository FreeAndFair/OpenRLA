import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardActions,
  CardHeader,
  CardTitle,
  CardText
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';

import _ from 'lodash';

import setPage from '../../action/setPage';
import isElectionDefined from '../../selector/isElectionDefined';


const cardActionsStyle = {
  display: 'flex',
  justifyContent: 'center',
  paddingTop: '50px',
};


const ElectionInfoCard = ({ election, electionDefined, navigateElection, style }) => {
  let buttonLabel;
  let status;

  if (!electionDefined) {
    buttonLabel = 'Edit Election';
    status = 'Election definition is incomplete. Upload manifests for contests, candidates, and cast vote records.';
  } else {
    buttonLabel = 'View Election';
    status = 'Election is fully defined. View the data for the uploaded manifests.';
  }

  return (
    <Card style={style}>
      <CardHeader>
        <CardTitle
           title="Election"
           subtitle="Define or edit the current election" />
      </CardHeader>
      <CardText>
        <p>{status}</p>
      </CardText>
      <CardActions style={cardActionsStyle}>
        <RaisedButton label={buttonLabel} onClick={navigateElection} />
      </CardActions>
    </Card>
  );
};

ElectionInfoCard.propTypes = {
  election: PropTypes.object.isRequired,
  electionDefined: PropTypes.bool.isRequired,
  navigateElection: PropTypes.func.isRequired,
};

const mapStateToProps = ({ election }) => ({
  election,
  electionDefined: isElectionDefined(election),
});

const mapDispatchToProps = dispatch => ({
  navigateElection: () => dispatch(setPage('election')),
});

export default connect(mapStateToProps, mapDispatchToProps)(ElectionInfoCard);
