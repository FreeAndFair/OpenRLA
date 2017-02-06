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
import Slider from 'material-ui/Slider';
import TextField from 'material-ui/TextField';


const defaultRiskLimit = 0.05;


class DefineAudit extends React.Component {
  constructor(props) {
    super(props);

    this.state = { riskLimit: defaultRiskLimit };

    ['onDateChange', 'onSliderChange'].forEach(m => {
      this[m] = this[m].bind(this);
    });
  }

  onDateChange(_, dateObj) {
    const date = `${dateObj}`;
    this.setState({ date });
  }

  onSliderChange(_, riskLimit) {
    this.setState({ riskLimit });
  }

  render() {
    return (
      <Card>
        <List>
          <ListItem secondaryText='Date'>
            <DatePicker
               defaultDate={new Date()}
               onChange={this.onDateChange}
               id='auditDate'
               ref='auditDate' />
          </ListItem>
          <ListItem secondaryText='Risk Limit'>
            <Slider
               onChange={this.onSliderChange}
               value={this.state.riskLimit}
               style={{width: 100}}
               min={0.0001}
               max={0.5000}
               step={0.0001}
               id='riskLimitSlider'
               ref='riskLimitSlider' />
            <TextField
               value={this.state.riskLimit}
               id='riskLimitText'
               ref='riskLimitText' />
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
