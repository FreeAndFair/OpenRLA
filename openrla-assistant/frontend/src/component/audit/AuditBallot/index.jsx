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


class AuditBallot extends React.Component {
  constructor(props) {
    super(props);

    ['saveMarks'].forEach(m => {
      this[m] = this[m].bind(this);
    });
  }

  saveMarks() {}

  render() {
    const {
      audit,
      closeDialog,
      dialogOpen,
    } = this.props;

    const closeButton = (
      <FlatButton
         label="Close"
         primary={true}
         onTouchTap={closeDialog} />
    );
    const saveButton = (
      <RaisedButton
         label="Save"
         primary={true}
         onTouchTap={this.saveMarks} />
    );
    const actions = [closeButton, saveButton];

    return (
        <Dialog
          title="Audit ballot"
           actions={actions}
           modal={false}
           open={dialogOpen}
           onRequestClose={closeDialog}
           autoScrollBodyContent={true} >
        </Dialog>
    );
  }
}

AuditBallot.propTypes = {
  audit: PropTypes.object.isRequired,
  closeDialog: PropTypes.func.isRequired,
  dialogOpen: PropTypes.bool.isRequired,
};

const mapStateToProps = state => {
  const { audit } = state;
  return { audit };
};

const mapDispatchToProps = (dispatch, props) => {
  return {};
};

export default connect(mapStateToProps, mapDispatchToProps)(AuditBallot);
