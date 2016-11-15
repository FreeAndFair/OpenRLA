import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardActions,
  CardTitle,
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';


const CastVoteRecordsCard = ({ uploadCastVoteRecords, viewCastVoteRecords }) => {
  return (
    <Card>
      <CardTitle
         title='Cast Vote Records'
         subtitle='Upload or view the Cast Vote Records for the election.' />
      <CardActions>
        <RaisedButton label="Upload" onClick={uploadCastVoteRecords} />
        <RaisedButton label="View" onClick={viewCastVoteRecords} />
      </CardActions>
    </Card>
  );
};

CastVoteRecordsCard.propTypes = {
  uploadCastVoteRecords: PropTypes.func.isRequired,
  viewCastVoteRecords: PropTypes.func.isRequired,
};

const mapDispatchToProps = dispatch => ({
  uploadCastVoteRecords: () => {},
  viewCastVoteRecords: () => {},
});

export default connect(null, mapDispatchToProps)(CastVoteRecordsCard);
