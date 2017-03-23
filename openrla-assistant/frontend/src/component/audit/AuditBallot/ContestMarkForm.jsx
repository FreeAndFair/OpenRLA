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
          <TableRowColumn style={{ width: '50px' }}>{id}</TableRowColumn>
          <TableRowColumn style={{ width: '50px' }}>{externalId}</TableRowColumn>
          <TableRowColumn style={{ width: '150px' }}>{type}</TableRowColumn>
          <TableRowColumn style={{ width: '350px' }}>{description}</TableRowColumn>
        </TableRow>
      );
    };

    const sorted = _.sortBy(candidates, 'id');
    const rows = _.map(sorted, makeRow);
    rows.push(
      <TableRow key={'invalid'} selected={this.isSelected('invalid')}>
        <TableRowColumn style={{ width: '50px' }}></TableRowColumn>
        <TableRowColumn style={{ width: '50px' }}></TableRowColumn>
        <TableRowColumn style={{ width: '150px' }}></TableRowColumn>
        <TableRowColumn style={{ width: '350px' }}>Invalid</TableRowColumn>
      </TableRow>
    );

    return (
      <Card>
        <TextField
           style={{ width: '100px' }}
           floatingLabelText='Contest ID'
           value={contest.id} />
        <TextField
           style={{ width: '600px' }}
           floatingLabelText='Description'
           value={contest.description} />
        <TextField
           style={{ width: '100px' }}
           floatingLabelText='External ID'
           value={contest.externalId} />
        <Table onRowSelection={this.onRowSelection}>
          <TableHeader displaySelectAll={false}>
            <TableRow>
              <TableHeaderColumn style={{ width: '50px' }}>Candidate ID</TableHeaderColumn>
              <TableHeaderColumn style={{ width: '50px' }}>External ID</TableHeaderColumn>
              <TableHeaderColumn style={{ width: '150px' }}>Type</TableHeaderColumn>
              <TableHeaderColumn style={{ width: '350px' }}>Description</TableHeaderColumn>
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
