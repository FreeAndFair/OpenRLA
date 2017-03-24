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
import Divider from 'material-ui/Divider';
import RaisedButton from 'material-ui/RaisedButton';
import Subheader from 'material-ui/Subheader';
import TextField from 'material-ui/TextField';

import AuditBallot from './AuditBallot';
import ContestStats from './ContestStats';

import { formatPercent } from '../../util';


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
    let currentSampleControl;
    if (audit.sample) {
      currentSampleId = audit.sample.id;
      currentSampleControl = (
        <div>
          <TextField
             style={{ width: '150px' }}
             floatingLabelText='Current sample ID'
             value={currentSampleId} />
          <RaisedButton label='Audit' onClick={this.openDialog} />
        </div>
      );
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
           currentSampleControl={currentSampleControl}
           audit={audit}
           contests={contests}
           election={election} />
      );
    }

    const infoItemStyle = { width: '150px' };
    const layoutStyle = {
      display: 'flex',
      justifyContent: 'space-around',
    };

    const riskLimitPercent = formatPercent(audit.riskLimit, 1);
    const totalBallotCount = _.size(election.ballots);

    return (
      <div>
        <Divider />
        <Subheader>Audit Info</Subheader>
        <List style={layoutStyle}>
          <ListItem style={infoItemStyle}>
            <TextField
               style={infoItemStyle}
               floatingLabelText='Election ID'
               value={audit.electionId} />
          </ListItem>
          <ListItem style={infoItemStyle}>
            <TextField
               style={infoItemStyle}
               floatingLabelText='Audit ID'
               value={audit.id} />
          </ListItem>
          <ListItem style={infoItemStyle}>
            <DatePicker
               floatingLabelText='Date'
               value={new Date(audit.date)} />
          </ListItem>
          <ListItem style={infoItemStyle}>
            <TextField
               style={infoItemStyle}
               floatingLabelText='Risk limit'
               value={riskLimitPercent} />
          </ListItem>
          <ListItem style={infoItemStyle}>
            <TextField
               style={infoItemStyle}
               floatingLabelText='Total ballots'
               value={totalBallotCount} />
          </ListItem>
        </List>
        {contestStats}
        {auditBallot}
      </div>
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
