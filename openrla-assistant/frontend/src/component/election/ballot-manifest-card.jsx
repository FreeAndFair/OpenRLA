import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardActions,
  CardTitle,
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';


const BallotManifestCard = ({ uploadBallotManifest, viewBallotManifest }) => {
  return (
    <Card>
      <CardTitle
         title='Ballot Manifest'
         subtitle='Upload or view the ballot manifest for the election.' />
      <CardActions>
        <RaisedButton label="Upload" onClick={uploadBallotManifest} />
        <RaisedButton label="View" onClick={viewBallotManifest} />
      </CardActions>
    </Card>
  );
};

BallotManifestCard.propTypes = {
  uploadBallotManifest: PropTypes.func.isRequired,
  viewBallotManifest: PropTypes.func.isRequired,
};

const mapDispatchToProps = dispatch => ({
  uploadBallotManifest: () => {},
  viewBallotManifest: () => {},
});

export default connect(null, mapDispatchToProps)(BallotManifestCard);
