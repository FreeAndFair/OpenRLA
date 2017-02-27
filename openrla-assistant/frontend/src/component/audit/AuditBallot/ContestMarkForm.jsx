import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';

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

    this.state = { selected: null };

    ['isSelected', 'onRowSelection'].forEach(m => {
      this[m] = this[m].bind(this);
    });
  }

  onRowSelection(selectedRows) {
    const i = selectedRows[0];
    const sorted = _.sortBy(this.props.contest.candidates, 'id');
    sorted.push('invalid');
    const selected = sorted[i];

    this.setState({ selected });
  }

  isSelected(id) {
    const { selected } = this.state;

    if (!selected) return false;
    if (selected === 'invalid') return selected === id;

    return selected.id === id;
  }

  formData() {
    return {
      contestId: this.props.contest.id,
      candidateId: this.state.selected.id,
    };
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
        <TableRow key={id} selected={this.isSelected(id)}>
          <TableRowColumn>{id}</TableRowColumn>
          <TableRowColumn>{description}</TableRowColumn>
          <TableRowColumn>{externalId}</TableRowColumn>
          <TableRowColumn>{type}</TableRowColumn>
        </TableRow>
      );
    };

    const sorted = _.sortBy(candidates, 'id');
    const rows = _.map(sorted, makeRow);
    rows.push(
      <TableRow key={'invalid'} selected={this.isSelected('invalid')}>
        <TableRowColumn></TableRowColumn>
        <TableRowColumn>Invalid</TableRowColumn>
        <TableRowColumn></TableRowColumn>
        <TableRowColumn></TableRowColumn>
      </TableRow>
    );

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
             deselectOnClickaway={false}
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


export default ContestMarkForm;
