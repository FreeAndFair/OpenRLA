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
      ballotId,
      closeDialog,
      dialogOpen,
    } = this.props;
    const { contests } = audit;

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
      return (
        <ListItem key={c.id}>
          <ContestMarkForm contest={c} />
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
  ballotId: PropTypes.number.isRequired,
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
