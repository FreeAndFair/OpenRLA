import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import submitBallotImages from '../../../action/submitBallotImages';

import FileUploadCard from '../FileUploadCard';

import BallotImages from './BallotImages';


const BallotImagesCard = ({
  ballots,
  uploadDisabled,
  viewDisabled,
}) => {
  const uploadedDataEl = <BallotImages ballots={ballots} />;
  const title = 'Ballot Images';
  const subtitle = 'Upload or view the ballot image files for the election.';

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
  return {
    ballots,
    uploadDisabled: !_.isEmpty(ballots),
    viewDisabled: _.isEmpty(ballots),
  };
};

export default connect(mapStateToProps)(BallotImagesCard);
