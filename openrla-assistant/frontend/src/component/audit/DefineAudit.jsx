import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import {
  Card,
  CardTitle,
} from 'material-ui/Card';
import { List, ListItem } from 'material-ui/List';
import DatePicker from 'material-ui/DatePicker';
import RaisedButton from 'material-ui/RaisedButton';
import Slider from 'material-ui/Slider';
import {
  Table,
  TableBody,
  TableHeader,
  TableHeaderColumn,
  TableRow,
  TableRowColumn,
} from 'material-ui/Table';
import TextField from 'material-ui/TextField';


const defaultRiskLimit = 0.05;


class DefineAudit extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      date: new Date(),
      riskLimit: defaultRiskLimit,
      selectedContestIds: [],
    };

    ['onDateChange',
     'onRowSelection',
     'onSliderChange',
    ].forEach(m => {
      this[m] = this[m].bind(this);
    });
  }

  onDateChange(_, date) {
    this.setState({ date });
  }

  onRowSelection(selectedRows) {
    const { contests } = this.props;

    if (selectedRows === 'all') {
      const selectedContestIds = _.map(contests, c => c.id);
      return this.setState({ selectedContestIds });
    }

    const selectedContestIds = _.map(selectedRows, i => contests[i].id);

    this.setState({ selectedContestIds });
  }

  onSliderChange(_, riskLimit) {
    this.setState({ riskLimit });
  }

  render() {
    const { contests } = this.props;

    const makeRow = ({ id, externalId, description }) => {
      const selected = _.includes(this.state.selectedContestIds, id);

      return (
        <TableRow selected={selected} >
          <TableRowColumn>{id}</TableRowColumn>
          <TableRowColumn>{externalId}</TableRowColumn>
          <TableRowColumn>{description}</TableRowColumn>
        </TableRow>
      );
    };

    const rows = _.map(contests, makeRow);

    return (
      <Card>
        <List>
          <ListItem secondaryText='Date'>
            <DatePicker
               value={this.state.date}
               onChange={this.onDateChange}
               id='auditDate'
               ref='auditDate' />
          </ListItem>
          <ListItem secondaryText='Risk Limit'>
            <Slider
               onChange={this.onSliderChange}
               value={this.state.riskLimit}
               style={{width: 100}}
               min={0.001}
               max={0.500}
               step={0.001}
               id='riskLimitSlider'
               ref='riskLimitSlider' />
            <TextField
               value={this.state.riskLimit}
               id='riskLimitText'
               ref='riskLimitText' />
          </ListItem>
          <ListItem secondaryText='Contests to Audit'>
            <Table
               onRowSelection={this.onRowSelection}
               multiSelectable={true} >
              <TableHeader>
                <TableRow>
                  <TableHeaderColumn>ID</TableHeaderColumn>
                  <TableHeaderColumn>External ID</TableHeaderColumn>
                  <TableHeaderColumn>Description</TableHeaderColumn>
                </TableRow>
              </TableHeader>
              <TableBody
                 showRowHover={true}
                 children={rows} />
            </Table>
          </ListItem>
        </List>
      </Card>
    );
  }
}

DefineAudit.PropTypes = {
  contests: PropTypes.array.isRequired,
};

const sortById = coll => _.sortBy(coll, o => o.id);

const mapStateToProps = state => {
  const { election: { contests } } = state;

  return { contests: sortById(contests) };
};

export default connect(mapStateToProps)(DefineAudit);
