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

import computeASN from 'selector/computeASN';


class ContestStats extends React.Component {
  constructor(props) {
    super(props);
  }

  openDialog() {
    this.setState({ open: true });
  }

  closeDialog() {
    this.setState({ open: false });
  }

  render() {
    const { audit, contests, election } = this.props;

    const listItems = _.map(audit.contests, ({ id, statistic }) => {
      return (
        <ListItem key={id}>
          <TextField
             style={{ width: '100px' }}
             floatingLabelText='Contest ID'
             value={id} />
          <TextField
             style={{ width: '600px' }}
             floatingLabelText='Description'
             value={contests[id].description} />
          <TextField
             style={{ width: '200px' }}
             floatingLabelText='Risk Level Achieved'
             value={statistic} />
        </ListItem>
      );
    });

    const sampledBallotCount = audit.marks ? audit.marks.length : 0;

    const statItemStyle = { width: '120px' };
    const statListStyle = {
      display: 'flex',
      justifyContent: 'space-around',
    };
    const asn = computeASN(audit, election);
    const riskLevel = (1 / audit.riskLimit).toFixed(1);
    return (
      <div>
        <Divider />
        <Subheader>Audit Progress</Subheader>
        <div style={statListStyle}>
          <TextField
             style={statItemStyle}
             floatingLabelText='Sampled ballots'
             value={sampledBallotCount} />
          <TextField
             style={statItemStyle}
             floatingLabelText='Risk Level'
             value={riskLevel} />
          <TextField
             style={statItemStyle}
             floatingLabelText='Sample size'
             value={asn} />
          <TextField
             style={statItemStyle}
             floatingLabelText='Ballots Needed'
             value={asn - sampledBallotCount} />
          {this.props.currentSampleControl}
        </div>
        <div>
          <Divider />
          <Subheader>Contests</Subheader>
          <List>
            {listItems}
          </List>
        </div>
      </div>
    );
  }
}

ContestStats.PropTypes = {
  audit: PropTypes.object.isRequired,
  contests: PropTypes.object.isRequired,
  election: PropTypes.object.isRequired,
};

export default ContestStats;
