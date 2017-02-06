import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardTitle,
} from 'material-ui/Card';
import { List, ListItem } from 'material-ui/List';
import DatePicker from 'material-ui/DatePicker';
import RaisedButton from 'material-ui/RaisedButton';
import TextField from 'material-ui/TextField';


class DefineAudit extends React.Component {
  constructor(props) {
    super(props);

    ['onDateChange'].forEach(m => {
      this[m] = this[m].bind(this);
    });
  }

  onDateChange() {}

  render() {
    return (
      <Card>
        <List>
          <ListItem secondaryText='Date'>
            <DatePicker
               onChange={this.onDateChange}
               id='auditDate'
               ref='auditDate' />
            <DatePicker />
          </ListItem>
          <ListItem secondaryText='Risk Limit'>
            <TextField />
          </ListItem>
          <ListItem secondaryText='Contests to Audit'>
            <TextField />
          </ListItem>
        </List>
      </Card>
    );
  }
}

export default connect()(DefineAudit);
