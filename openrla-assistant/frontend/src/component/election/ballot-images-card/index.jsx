import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardActions,
  CardTitle,
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';


const BallotImagesCard = ({ uploadBallotImages, viewBallotImages }) => {
  return (
    <Card>
      <CardTitle
         title='Ballot Images'
         subtitle='Upload or view the ballot images for the election.' />
      <CardActions>
        <RaisedButton label="Upload" onClick={uploadBallotImages} />
        <RaisedButton label="View" onClick={viewBallotImages} />
      </CardActions>
    </Card>
  );
};

BallotImagesCard.propTypes = {
  uploadBallotImages: PropTypes.func.isRequired,
  viewBallotImages: PropTypes.func.isRequired,
};

const mapDispatchToProps = dispatch => ({
  uploadBallotImages: () => {},
  viewBallotImages: () => {},
});

export default connect(null, mapDispatchToProps)(BallotImagesCard);
