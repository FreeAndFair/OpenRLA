import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';
import { remote } from 'electron';

import {
  Card,
  CardActions,
  CardTitle,
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';

import ViewContestManifestButton from './view-button';


const ContestManifestCard = ({
  contests,
  uploadContestManifest,
}) => {
  return (
    <Card>
      <CardTitle
         title='Contest Manifest'
         subtitle='Upload or view the manifest for the contests in the election.' />
      <CardActions>
        <RaisedButton label="Upload" onClick={uploadContestManifest} />
        <ViewContestManifestButton contests={contests} />
      </CardActions>
    </Card>
  );
};

ContestManifestCard.propTypes = {
  contests: PropTypes.array.isRequired,
  uploadContestManifest: PropTypes.func.isRequired,
};

const mapDispatchToProps = dispatch => ({
  uploadContestManifest: () => {
    const options = { properties: ['openFile'] };
    const cb = console.log;
    remote.dialog.showOpenDialog(options, cb);
  },
});

export default connect(null, mapDispatchToProps)(ContestManifestCard);
