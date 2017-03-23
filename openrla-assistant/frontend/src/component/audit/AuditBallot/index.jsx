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
import { List, ListItem } from 'material-ui/List';
import RaisedButton from 'material-ui/RaisedButton';
import TextField from 'material-ui/TextField';

import ContestMarkForm from './ContestMarkForm';

import submitAuditMarks from 'action/submitAuditMarks';


class AuditBallot extends React.Component {
  constructor(props) {
    super(props);

    this.contestMarkForms = {};

    ['saveMarks'].forEach(m => {
      this[m] = this[m].bind(this);
    });
  }

  saveMarks() {
    const { ballotId } = this.props;
    const marks = _.map(this.contestMarkForms, f => f.formData());
    const data = { ballotId, marks };
    this.props.submitAuditMarks(this.props.audit.id, data);
    this.props.closeDialog();
  }

  render() {
    const {
      audit,
      ballotId,
      contests,
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

    const contestForms = _.map(contests, c => {
      const ref = f => {
        this.contestMarkForms[c.id] = f;
      };
      return (
        <ListItem key={c.id}>
          <ContestMarkForm
             contest={c}
             ref={ref} />
        </ListItem>
      );
    });

    return (
      <Dialog
         title="Audit ballot"
         actions={actions}
         modal={false}
         open={dialogOpen}
         onRequestClose={closeDialog}
         autoScrollBodyContent={true} >
        <TextField
           floatingLabelText='Ballot ID'
           value={ballotId} />
        <List>
          {contestForms}
        </List>
      </Dialog>
    );
  }
}

AuditBallot.propTypes = {
  audit: PropTypes.object.isRequired,
  contests: PropTypes.array.isRequired,
  ballotId: PropTypes.number.isRequired,
  closeDialog: PropTypes.func.isRequired,
  dialogOpen: PropTypes.bool.isRequired,
};

const mapStateToProps = state => {
  const { audit, election } = state;

  const contests = _.map(audit.contests, c => election.contests[c.id]);

  return { audit, contests };
};

const mapDispatchToProps = (dispatch, props) => {
  return {
    submitAuditMarks: (id, data) => submitAuditMarks(id, data)(dispatch),
  };
};

export default connect(mapStateToProps, mapDispatchToProps)(AuditBallot);
