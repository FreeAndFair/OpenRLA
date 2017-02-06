import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import BallotImagesCard from './ballot-images-card';
import CandidateManifestCard from './candidate-manifest-card';
import ContestManifestCard from './contest-manifest-card';
import ElectionSummary from './summary';


const Election = ({ election }) => {
  return (
    <div>
      <ElectionSummary election={election} />
      <ContestManifestCard contests={election.contests} />
      <CandidateManifestCard />
      <BallotImagesCard />
    </div>
  );
};

Election.propTypes = {
  election: PropTypes.object.isRequired,
};

const mapStateToProps = ({ election }) => ({ election });

export default connect(mapStateToProps)(Election);
