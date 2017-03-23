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
    const totalBallotCount = _.size(election.ballots);

    return (
      <div>
        <TextField
           floatingLabelText='Sampled ballots'
           value={sampledBallotCount} />
        <TextField
           floatingLabelText='Total ballots'
           value={totalBallotCount} />
        <TextField
           floatingLabelText='Risk Level'
           value={1 / audit.riskLimit} />
        <List>
          {listItems}
        </List>
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
