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
const extIdColStyle = { width: '50px' };
const typeColStyle = { width: '100px' };


const CandidateManifest = ({ candidates }) => {
  const makeRow = ({ id, externalId, description, contestId, type }) => (
    <TableRow>
      <TableRowColumn style={idColStyle}>{id}</TableRowColumn>
      <TableRowColumn style={extIdColStyle}>{externalId}</TableRowColumn>
      <TableRowColumn>{description}</TableRowColumn>
      <TableRowColumn style={idColStyle}>{contestId}</TableRowColumn>
      <TableRowColumn style={typeColStyle}>{type}</TableRowColumn>
    </TableRow>
  );

  const rows = _.map(candidates, makeRow);

  return (
    <div>
      <Table selectable={false}>
        <TableHeader adjustForCheckbox={false} displaySelectAll={false}>
          <TableRow>
            <TableHeaderColumn style={idColStyle}>ID</TableHeaderColumn>
            <TableHeaderColumn style={extIdColStyle}>External ID</TableHeaderColumn>
            <TableHeaderColumn>Description</TableHeaderColumn>
            <TableHeaderColumn style={idColStyle}>ContestId</TableHeaderColumn>
            <TableHeaderColumn style={typeColStyle}>Type</TableHeaderColumn>
          </TableRow>
        </TableHeader>
        <TableBody displayRowCheckbox={false} children={rows} />
      </Table>
    </div>
  );
};

CandidateManifest.PropTypes = {
  candidates: PropTypes.array.isRequired,
};


export default connect()(CandidateManifest);
