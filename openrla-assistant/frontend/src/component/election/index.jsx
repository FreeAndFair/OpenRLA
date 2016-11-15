import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import BallotImagesCard from './ballot-images-card';
import BallotManifestCard from './ballot-manifest-card';
import CastVoteRecordsCard from './cast-vote-records-card';
import ContestManifestCard from './contest-manifest-card';
import ElectionSummary from './election-summary';


const Election = ({ election }) => {
  return (
    <div>
      <ElectionSummary election={election} />
      <ContestManifestCard />
      <BallotManifestCard election={election} />
      <CastVoteRecordsCard election={election} />
      <BallotImagesCard />
    </div>
  );
};

Election.propTypes = {
  election: PropTypes.object.isRequired,
};

const mapStateToProps = ({ election }) => ({ election });

export default connect(mapStateToProps)(Election);
