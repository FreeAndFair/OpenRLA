import React, { PropTypes } from 'react';

import _ from 'lodash';

import {
  Table,
  TableBody,
  TableHeader,
  TableHeaderColumn,
  TableRow,
  TableRowColumn,
} from 'material-ui/Table';


const Archive = ({ archive }) => {
  const makeRow = ({election, date, riskLimit, stages}) => (
    <TableRow>
      <TableRowColumn>{date}</TableRowColumn>
      <TableRowColumn>{election.title}</TableRowColumn>
      <TableRowColumn>{stages}</TableRowColumn>
      <TableRowColumn>{riskLimit}</TableRowColumn>
    </TableRow>
  );

  console.log('archive', archive);
  const rows = _.map(archive, makeRow);

  console.log('rows', rows);

  return (
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
  );
};

Archive.PropTypes = {
  archive: PropTypes.array.isRequired,
};


export default Archive;
