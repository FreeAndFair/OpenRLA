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


const ContestManifest = ({ contests }) => {
  const makeRow = ({ id, externalId, description }) => (
    <TableRow>
      <TableRowColumn>{id}</TableRowColumn>
      <TableRowColumn>{externalId}</TableRowColumn>
      <TableRowColumn>{description}</TableRowColumn>
    </TableRow>
  );

  const rows = _.map(contests, makeRow);

  return (
    <div>
      <Table selectable={false}>
        <TableHeader displaySelectAll={false}>
          <TableRow>
            <TableHeaderColumn>ID</TableHeaderColumn>
            <TableHeaderColumn>External ID</TableHeaderColumn>
            <TableHeaderColumn>Description</TableHeaderColumn>
          </TableRow>
        </TableHeader>
        <TableBody displayRowCheckbox={false} children={rows} />
      </Table>
    </div>
  );
};

ContestManifest.PropTypes = {
  contests: PropTypes.object.isRequired,
};


export default connect()(ContestManifest);
