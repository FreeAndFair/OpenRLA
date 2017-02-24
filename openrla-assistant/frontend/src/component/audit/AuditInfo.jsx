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
import ContestStats from './ContestStats';


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
    const { audit, contests, election } = this.props;

    let currentSampleId;
    if (audit.sample) {
      currentSampleId = audit.sample.id;
    }

    let auditBallot;
    let contestStats;
    if (!_.isEmpty(contests)) {
      auditBallot = (
        <AuditBallot
           ballotId={currentSampleId}
           closeDialog={this.closeDialog}
           dialogOpen={this.state.open} />
      );
      contestStats = (
        <ContestStats
           audit={audit}
           contests={contests}
           election={election} />
      );
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
        {contestStats}
        {auditBallot}
      </Card>
    );
  }
}

AuditInfo.PropTypes = {
  audit: PropTypes.object.isRequired,
  contests: PropTypes.object.isRequired,
  election: PropTypes.object.isRequired,
};

const mapStateToProps = state => {
  const { audit, election } = state;
  const { contests } = election;

  return { audit, contests, election };
};

const mapDispatchToProps = dispatch => {
  return {};
};


export default connect(mapStateToProps, mapDispatchToProps)(AuditInfo);
