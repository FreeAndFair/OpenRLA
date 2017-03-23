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


const dialogStyle = {
  maxWidth: 'none',
  width: '95%',
};


class FileUploadCard extends React.Component {
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
      chooseFiles,
      subtitle,
      title,
      uploadDisabled,
      uploadedDataEl,
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
          <RaisedButton
             disabled={uploadDisabled}
             label="Upload"
             onClick={chooseFiles} />
          <RaisedButton
             disabled={viewDisabled}
             label="View"
             onTouchTap={this.openDialog} >
          </RaisedButton>
          <Dialog
             contentStyle={dialogStyle}
             title="Contest Files"
             actions={actions}
             modal={false}
             open={this.state.open}
             onRequestClose={this.closeDialog}
             autoScrollBodyContent={true} >
            { uploadedDataEl }
          </Dialog>
        </CardActions>
      </Card>
    );
  }
}

FileUploadCard.propTypes = {
  chooseFiles: PropTypes.func.isRequired,
  multiSelections: PropTypes.bool.isRequired,
  submitFiles: PropTypes.func.isRequired,
  subtitle: PropTypes.string.isRequired,
  title: PropTypes.string.isRequired,
  uploadDisabled: PropTypes.bool.isRequired,
  uploadedDataEl: PropTypes.element.isRequired,
  viewDisabled: PropTypes.bool.isRequired,
};

const mapDispatchToProps = (dispatch, props) => {
  const { multiSelections, submitFiles } = props;

  return {
    chooseFiles: () => {
      const options = { properties: ['openFile'] };

      if (multiSelections) {
        options.properties.push('multiSelections');
      }

      remote.dialog.showOpenDialog(options, filePaths => {
        if (filePaths) {
          dispatch(submitFiles(filePaths));
        }
      });
    },
  };
};

export default connect(null, mapDispatchToProps)(FileUploadCard);
