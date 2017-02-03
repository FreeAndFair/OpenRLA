import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardActions,
  CardTitle,
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';


const CandidateManifestCard = ({
  uploadCandidateManifest,
  viewCandidateManifest
}) => {
  return (
    <Card>
      <CardTitle
         title='Candidate Manifest'
         subtitle='Upload or view the candidate manifest for the election.' />
      <CardActions>
        <RaisedButton label="Upload" onClick={uploadCandidateManifest} />
        <RaisedButton label="View" onClick={viewCandidateManifest} />
      </CardActions>
    </Card>
  );
};

CandidateManifestCard.propTypes = {
  uploadCandidateManifest: PropTypes.func.isRequired,
  viewCandidateManifest: PropTypes.func.isRequired,
};

const mapDispatchToProps = dispatch => ({
  uploadCandidateManifest: () => {},
  viewCandidateManifest: () => {},
});

export default connect(null, mapDispatchToProps)(CandidateManifestCard);
