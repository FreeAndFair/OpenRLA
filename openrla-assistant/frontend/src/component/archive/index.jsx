import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import RaisedButton from 'material-ui/RaisedButton';
import {
  Table,
  TableBody,
  TableHeader,
  TableHeaderColumn,
  TableRow,
  TableRowColumn,
} from 'material-ui/Table';


const Archive = ({ archive, tryRestartAudit }) => {
  const makeRow = ({election, date, riskLimit, stages}) => (
    <TableRow>
      <TableRowColumn>{date}</TableRowColumn>
      <TableRowColumn>{election.title}</TableRowColumn>
      <TableRowColumn>{stages}</TableRowColumn>
      <TableRowColumn>{riskLimit}</TableRowColumn>
    </TableRow>
  );

  const rows = _.map(archive, makeRow);

  return (
    <div>
      <RaisedButton label='Restart selected audit' onClick={tryRestartAudit} />
      <Table>
        <TableHeader displaySelectAll={false}>
          <TableRow>
            <TableHeaderColumn>Date</TableHeaderColumn>
            <TableHeaderColumn>Election</TableHeaderColumn>
            <TableHeaderColumn>Stages</TableHeaderColumn>
            <TableHeaderColumn>Risk-limit</TableHeaderColumn>
          </TableRow>
        </TableHeader>
        <TableBody children={rows} />
      </Table>
    </div>
  );
};

Archive.PropTypes = {
  archive: PropTypes.array.isRequired,
};

const mapDispatchToProps = dispatch => ({
  tryRestartAudit: () => {},
});


export default connect(null, mapDispatchToProps)(Archive);
