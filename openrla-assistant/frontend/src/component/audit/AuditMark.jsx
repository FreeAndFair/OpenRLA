import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import { Card } from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';
import {
  Table,
  TableBody,
  TableHeader,
  TableHeaderColumn,
  TableRow,
  TableRowColumn,
} from 'material-ui/Table';
import TextField from 'material-ui/TextField';


class AuditMark extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { ballotMark, election } = this.props;

    const makeRow = ({ contestId, candidateId }) => {
      const contest = election.contests[contestId];
      const candidate = contest.candidates[candidateId];

      if (!candidate) {
        return (
          <TableRow key={`${contestId}-invalid`}>
            <TableRowColumn style={{ width: '100px' }}>{contestId}</TableRowColumn>
            <TableRowColumn style={{ width: '100px' }}></TableRowColumn>
            <TableRowColumn>Invalid</TableRowColumn>
          </TableRow>
        );
      } else {
        return (
          <TableRow key={`${contestId}-${candidateId}`}>
            <TableRowColumn style={{ width: '100px' }}>{contestId}</TableRowColumn>
            <TableRowColumn style={{ width: '100px' }}>{candidateId}</TableRowColumn>
            <TableRowColumn>{candidate.description}</TableRowColumn>
          </TableRow>
        );
      }
    };

    const { ballotId, marks } = ballotMark;
    const rows = _.map(marks, makeRow);

    return (
      <Card>
        <TextField floatingLabelText='Ballot ID' value={ballotId} />
        <Table selectable={false}>
          <TableHeader adjustForCheckbox={false} displaySelectAll={false}>
            <TableRow>
              <TableHeaderColumn style={{ width: '100px' }}>Contest ID</TableHeaderColumn>
              <TableHeaderColumn style={{ width: '100px' }}>Candidate ID</TableHeaderColumn>
              <TableHeaderColumn>Candidate</TableHeaderColumn>
            </TableRow>
          </TableHeader>
          <TableBody displayRowCheckbox={false}>
            {rows}
          </TableBody>
        </Table>
      </Card>
    );
  }
}

AuditMark.PropTypes = {
  ballotMark: PropTypes.object.isRequired,
  election: PropTypes.object.isRequired,
};

const mapStateToProps = state => {
  return ({ election }) => ({ election });
};

const mapDispatchToProps = dispatch => {
  return {};
};


export default connect(mapStateToProps, mapDispatchToProps)(AuditMark);
