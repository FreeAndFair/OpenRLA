import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardActions,
  CardText,
  CardTitle,
} from 'material-ui/Card';
import Dialog from 'material-ui/Dialog';
import FlatButton from 'material-ui/FlatButton';
import RaisedButton from 'material-ui/RaisedButton';

import DefineAudit from './DefineAudit';


class NoAudit extends React.Component {
  constructor(props) {
    super(props);

    this.state = { open: false };

    [
      'openDialog',
      'closeDialog',
      'saveAudit',
    ].forEach(m => {
      this[m] = this[m].bind(this);
    });
  }

  openDialog() {
    this.setState({ open: true });
  };

  closeDialog() {
    this.setState({ open: false });
  };

  saveAudit() {
  }

  render() {
    const cancelButton = (
      <FlatButton
         label="Cancel"
         primary={true}
         onTouchTap={this.closeDialog} />
    );
    const saveButton = (
      <RaisedButton
         label="Save"
         primary={true}
         onTouchTap={this.saveAudit} />
    );
    const actions = [cancelButton, saveButton];

    return (
      <Card>
        <CardTitle title='Audit' />
        <CardText>
          No active audit.
        </CardText>
        <CardActions>
          <RaisedButton
             label='Start'
             onClick={this.openDialog} />
          <RaisedButton
             disabled={true}
             label='Archive'
             onClick={console.log} />
        </CardActions>
        <Dialog
           title="Define new audit"
           actions={actions}
           modal={false}
           open={this.state.open}
           onRequestClose={this.closeDialog}
           autoScrollBodyContent={true} >
          <DefineAudit />
        </Dialog>
      </Card>
    );
  }
}

export default connect()(NoAudit);
