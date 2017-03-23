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


const idColStyle = { width: '50px' };


const BallotManifest = ({ ballots }) => {
  const makeRow = ({ id, filePath, srcPath }) => (
    <TableRow>
      <TableRowColumn style={idColStyle}>{id}</TableRowColumn>
      <TableRowColumn>{srcPath}</TableRowColumn>
    </TableRow>
  );

  const rows = _.map(ballots, makeRow);

  return (
    <div>
      <Table selectable={false}>
        <TableHeader adjustForCheckbox={false} displaySelectAll={false}>
          <TableRow>
            <TableHeaderColumn style={idColStyle}>ID</TableHeaderColumn>
            <TableHeaderColumn>Path</TableHeaderColumn>
          </TableRow>
        </TableHeader>
        <TableBody displayRowCheckbox={false} children={rows} />
      </Table>
    </div>
  );
};

BallotManifest.PropTypes = {
  ballots: PropTypes.object.isRequired,
};


export default connect()(BallotManifest);
