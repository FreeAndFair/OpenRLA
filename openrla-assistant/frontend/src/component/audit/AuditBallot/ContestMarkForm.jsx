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


class ContestMarkForm extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { contest } = this.props;
    const { candidates } = contest;

    const makeRow = ({
      id,
      type,
      externalId,
      description,
    }) => {
      return (
        <TableRow key={id}>
          <TableRowColumn>{id}</TableRowColumn>
          <TableRowColumn>{description}</TableRowColumn>
          <TableRowColumn>{externalId}</TableRowColumn>
          <TableRowColumn>{type}</TableRowColumn>
        </TableRow>
      );
    };

    const rows = _.map(candidates, makeRow);

    return (
      <Card>
        <TextField
           floatingLabelText='Contest ID'
           value={contest.id} />
        <TextField
           floatingLabelText='Description'
           value={contest.description} />
        <TextField
           floatingLabelText='External ID'
           value={contest.externalId} />
        <Table onRowSelection={this.onRowSelection}>
          <TableHeader displaySelectAll={false}>
            <TableRow>
              <TableHeaderColumn>Candidate ID</TableHeaderColumn>
              <TableHeaderColumn>Description</TableHeaderColumn>
              <TableHeaderColumn>External ID</TableHeaderColumn>
              <TableHeaderColumn>Type</TableHeaderColumn>
            </TableRow>
          </TableHeader>
          <TableBody
             showRowHover={true}
             children={rows} />
        </Table>
      </Card>
    );
  }
}

ContestMarkForm.PropTypes = {
  contest: PropTypes.object.isRequired,
};

const mapStateToProps = state => {
  return {};
};

const mapDispatchToProps = dispatch => {
  return {};
};


export default connect(mapStateToProps, mapDispatchToProps)(ContestMarkForm);
