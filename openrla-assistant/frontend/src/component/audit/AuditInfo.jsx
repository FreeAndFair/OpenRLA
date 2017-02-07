import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import {
  Card,
  CardTitle,
  CardText
} from 'material-ui/Card';
import { List, ListItem } from 'material-ui/List';
import DatePicker from 'material-ui/DatePicker';
import RaisedButton from 'material-ui/RaisedButton';
import TextField from 'material-ui/TextField';

import AuditBallot from './AuditBallot';


class AuditInfo extends React.Component {
  constructor(props) {
    super(props);

    this.state = { open: false };

    ['openDialog', 'closeDialog'].forEach(m => {
      this[m] = this[m].bind(this);
    });
  }

  openDialog() {
    this.setState({ open: true });
  }

  closeDialog() {
    this.setState({ open: false });
  }

  render() {
    const { audit } = this.props;

    let currentSampleId;
    if (audit.sample) {
      currentSampleId = audit.sample.id;
    }

    return (
      <Card>
        <List>
          <ListItem>
            <TextField
               floatingLabelText='Election ID'
               value={audit.electionId} />
          </ListItem>
          <ListItem>
            <TextField
               floatingLabelText='Audit ID'
               value={audit.id} />
          </ListItem>
          <ListItem>
            <DatePicker
               floatingLabelText='Date'
               value={new Date(audit.date)} />
          </ListItem>
          <ListItem>
            <TextField
               floatingLabelText='Risk limit'
               value={audit.riskLimit} />
          </ListItem>
          <ListItem>
            <TextField
               floatingLabelText='Current sample ID'
               value={currentSampleId} />
            <RaisedButton label='Audit' onClick={this.openDialog} />
          </ListItem>
        </List>
        <AuditBallot
           ballotId={currentSampleId}
           closeDialog={this.closeDialog}
           dialogOpen={this.state.open} />
      </Card>
    );
  }
}

AuditInfo.PropTypes = {};

const mapStateToProps = state => {
  const { audit } = state;
  return { audit };
};

const mapDispatchToProps = dispatch => {
  return {};
};


export default connect(mapStateToProps, mapDispatchToProps)(AuditInfo);
