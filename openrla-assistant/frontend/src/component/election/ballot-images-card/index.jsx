import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import submitBallotImages from '../../../action/submitBallotImages';

import FileUploadCard from '../file-upload-card';

import BallotImages from './ballot-images';


const BallotImagesCard = ({ ballots }) => {
  const uploadedDataEl = <BallotImages ballots={ballots} />;
  const title = 'Ballot Images';
  const subtitle = 'Upload or view the ballot image files for the election.';
  const uploadDisabled = !_.isEmpty(ballots);
  const viewDisabled = _.isEmpty(ballots);

  return (
    <FileUploadCard
       multiSelections={true}
       uploadedDataEl={uploadedDataEl}
       title={title}
       subtitle={subtitle}
       submitFiles={submitBallotImages}
       uploadDisabled={uploadDisabled}
       viewDisabled={viewDisabled} />
  );
};

BallotImagesCard.propTypes = {
  ballots: PropTypes.object.isRequired,
};

const mapStateToProps = state => {
  const { election: { ballots } } = state;
  return { ballots };
};

export default connect(mapStateToProps)(BallotImagesCard);
