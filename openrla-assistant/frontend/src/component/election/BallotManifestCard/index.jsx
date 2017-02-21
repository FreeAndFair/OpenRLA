import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import {
  Card,
  CardActions,
  CardTitle,
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';

import submitBallotManifest from 'action/submitBallotManifest';

import BallotManifest from './BallotManifest';
import FileUploadCard from '../FileUploadCard';


const BallotManifestCard = ({
  ballots,
  submitBallotManifest,
  uploadDisabled,
  viewDisabled,
  viewBallotManifest,
}) => {
  const uploadedDataEl = <BallotManifest ballots={ballots} />;
  const title = 'Ballot Manifest';
  const subtitle = 'Upload or view the ballot manifest for the election.';

  return (
    <FileUploadCard
       multiSelections={false}
       uploadedDataEl={uploadedDataEl}
       title={title}
       subtitle={subtitle}
       submitFiles={submitBallotManifest}
       uploadDisabled={uploadDisabled}
       viewDisabled={viewDisabled} />
  );
};

BallotManifestCard.propTypes = {
  submitBallotManifest: PropTypes.func.isRequired,
  viewBallotManifest: PropTypes.func.isRequired,
};

const mapDispatchToProps = dispatch => ({
  submitBallotManifest: () => {},
  viewBallotManifest: () => {},
});

const mapStateToProps = state => {
  const { election } = state;
  const { ballots, id } = election;

  const uploadDisabled = !id || !_.isEmpty(ballots);
  const viewDisabled = _.isEmpty(ballots);

  return {
    ballots,
    uploadDisabled,
    viewDisabled,
  };
};

export default connect(mapStateToProps, mapDispatchToProps)(BallotManifestCard);
