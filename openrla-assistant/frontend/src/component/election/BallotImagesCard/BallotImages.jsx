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


const BallotImages = ({ ballots }) => {
  const makeRow = ({ id, filePath }) => (
    <TableRow>
      <TableRowColumn>{id}</TableRowColumn>
      <TableRowColumn>{filePath}</TableRowColumn>
    </TableRow>
  );

  const rows = _.map(ballots, makeRow);

  return (
    <div>
      <Table selectable={false}>
        <TableHeader displaySelectAll={false}>
          <TableRow>
            <TableHeaderColumn>ID</TableHeaderColumn>
            <TableHeaderColumn>Path</TableHeaderColumn>
          </TableRow>
        </TableHeader>
        <TableBody displayRowCheckbox={false} children={rows} />
      </Table>
    </div>
  );
};

BallotImages.PropTypes = {
  ballots: PropTypes.object.isRequired,
};


export default connect()(BallotImages);
