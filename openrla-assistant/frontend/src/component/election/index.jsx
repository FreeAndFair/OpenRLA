import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import BallotManifestCard from './BallotManifestCard';
import BallotImagesCard from './BallotImagesCard';
import CandidateManifestCard from './CandidateManifestCard';
import ContestManifestCard from './ContestManifestCard';
import ElectionSummary from './summary';


const Election = ({ election }) => {
  return (
    <div>
      <ElectionSummary election={election} />
      <ContestManifestCard />
      <CandidateManifestCard />
      <BallotManifestCard />
      <BallotImagesCard />
    </div>
  );
};

Election.propTypes = {
  election: PropTypes.object.isRequired,
};

const mapStateToProps = ({ election }) => ({ election });

export default connect(mapStateToProps)(Election);
