import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';
import { remote } from 'electron';

import _ from 'lodash';

import {
  Card,
  CardActions,
  CardTitle,
} from 'material-ui/Card';
import Dialog from 'material-ui/Dialog';
import FlatButton from 'material-ui/FlatButton';
import RaisedButton from 'material-ui/RaisedButton';


class ManifestCard extends React.Component {
  constructor(props) {
    super(props);
    this.state = { open: false };

    ['openDialog', 'closeDialog'].forEach(m => {
      this[m] = this[m].bind(this);
    });
  }

  openDialog() {
    this.setState({ open: true });
  };

  closeDialog() {
    this.setState({ open: false });
  };

  render() {
    const {
      chooseManifest,
      manifestEl,
      submitManifest,
      subtitle,
      title,
      viewDisabled,
    } = this.props;

    const closeButton = (
      <FlatButton
         label="Close"
         primary={true}
         onTouchTap={this.closeDialog} />
    );
    const actions = [closeButton];

    return (
      <Card>
        <CardTitle
           title={title}
           subtitle={subtitle} />
        <CardActions>
          <RaisedButton label="Upload" onClick={chooseManifest(submitManifest)} />
          <RaisedButton
             disabled={viewDisabled}
             label="View"
             onTouchTap={this.openDialog} >
          </RaisedButton>
          <Dialog
             title="Contest Manifest"
             actions={actions}
             modal={false}
             open={this.state.open}
             onRequestClose={this.closeDialog}
             autoScrollBodyContent={true} >
            { manifestEl }
          </Dialog>
        </CardActions>
      </Card>
    );
  }
}

ManifestCard.propTypes = {
  chooseManifest: PropTypes.func.isRequired,
  manifestEl: PropTypes.element.isRequired,
  submitManifest: PropTypes.func.isRequired,
  subtitle: PropTypes.string.isRequired,
  title: PropTypes.string.isRequired,
  viewDisabled: PropTypes.bool.isRequired,
};

const mapDispatchToProps = dispatch => ({
  chooseManifest: submitManifest => () => {
    const options = { properties: ['openFile'] };

    remote.dialog.showOpenDialog(options, filePaths => {
      if (filePaths) {
        dispatch(submitManifest(filePaths[0]));
      }
    });
  },
});

export default connect(null, mapDispatchToProps)(ManifestCard);
