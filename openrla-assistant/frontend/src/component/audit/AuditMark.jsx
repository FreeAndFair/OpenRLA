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
      return (
        <TableRow key={`${contestId}-${candidateId}`}>
          <TableRowColumn>{contestId}</TableRowColumn>
          <TableRowColumn>{candidateId}</TableRowColumn>
        </TableRow>
      );
    };

    const { ballotId, marks } = ballotMark;
    const rows = _.map(marks, makeRow);

    return (
      <Card>
        <TextField floatingLabelText='Ballot ID' value={ballotId} />
        <Table selectable={false}>
          <TableHeader displaySelectAll={false}>
            <TableRow>
              <TableHeaderColumn>Contest ID</TableHeaderColumn>
              <TableHeaderColumn>Candidate ID</TableHeaderColumn>
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
