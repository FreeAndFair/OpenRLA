import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardActions,
  CardTitle,
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';


const ContestManifestCard = ({ uploadContestManifest, viewContestManifest }) => {
  return (
    <Card>
      <CardTitle
         title='Contest Manifest'
         subtitle='Upload or view the manifest for the contests in the election.' />
      <CardActions>
        <RaisedButton label="Upload" onClick={uploadContestManifest} />
        <RaisedButton label="View" onClick={viewContestManifest} />
      </CardActions>
    </Card>
  );
};

ContestManifestCard.propTypes = {
  uploadContestManifest: PropTypes.func.isRequired,
  viewContestManifest: PropTypes.func.isRequired,
};

const mapDispatchToProps = dispatch => ({
  uploadContestManifest: () => {},
  viewContestManifest: () => {},
});

export default connect(null, mapDispatchToProps)(ContestManifestCard);
