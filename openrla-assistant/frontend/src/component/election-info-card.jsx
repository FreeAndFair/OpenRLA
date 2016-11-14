import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardActions,
  CardTitle,
  CardText
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';

import _ from 'lodash';

import setPage from '../action/setPage';
import isElectionDefined from '../selector/is-election-defined';


const ElectionInfoCard = ({ election, electionDefined, navigateElection }) => {
  let status;

  if (_.isEmpty(election)) {
    status = <div>Election is not defined.</div>;
  } else if (!electionDefined) {
    status = <div>Election definition is incomplete.</div>;
  } else {
    status = <div>Election is defined.</div>
  }

  return (
    <Card>
      <CardTitle
         title="Election"
         subtitle="Define or edit the current election" />
      <CardText>
        Identify a single election's data files, including:
        <ul>
          <li>Election metadata</li>
          <li>Ballot manifest</li>
          <li>Cast vote records</li>
          <li>Ballot images</li>
        </ul>
      </CardText>
      <CardActions>
        <RaisedButton label="Edit Election" onClick={navigateElection} />
      </CardActions>
    </Card>
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
  navigateElection: () => dispatch(setPage('election')),
});

export default connect(mapStateToProps, mapDispatchToProps)(ElectionInfoCard);
