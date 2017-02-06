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


const CandidateManifest = ({ candidates }) => {
  const makeRow = ({ id, externalId, description, contestId, type }) => (
    <TableRow>
      <TableRowColumn>{id}</TableRowColumn>
      <TableRowColumn>{externalId}</TableRowColumn>
      <TableRowColumn>{description}</TableRowColumn>
      <TableRowColumn>{contestId}</TableRowColumn>
      <TableRowColumn>{type}</TableRowColumn>
    </TableRow>
  );

  const rows = _.map(candidates, makeRow);

  return (
    <div>
      <Table selectable={false}>
        <TableHeader displaySelectAll={false}>
          <TableRow>
            <TableHeaderColumn>ID</TableHeaderColumn>
            <TableHeaderColumn>External ID</TableHeaderColumn>
            <TableHeaderColumn>Description</TableHeaderColumn>
            <TableHeaderColumn>ContestId</TableHeaderColumn>
            <TableHeaderColumn>Type</TableHeaderColumn>
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
