import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import {
  Card,
  CardTitle,
  CardText
} from 'material-ui/Card';
import { List, ListItem } from 'material-ui/List';
import DatePicker from 'material-ui/DatePicker';
import RaisedButton from 'material-ui/RaisedButton';
import TextField from 'material-ui/TextField';


class AuditInfo extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { audit } = this.props;

    let currentSampleId;
    if (audit.sample) {
      currentSampleId = audit.sample.id;
    }

    return (
      <Card>
        <List>
          <ListItem>
            <TextField
               floatingLabelText='Election ID'
               value={audit.electionId} />
          </ListItem>
          <ListItem>
            <TextField
               floatingLabelText='Audit ID'
               value={audit.id} />
          </ListItem>
          <ListItem>
            <DatePicker
               floatingLabelText='Date'
               value={new Date(audit.date)} />
          </ListItem>
          <ListItem>
            <TextField
               floatingLabelText='Risk limit'
               value={audit.riskLimit} />
          </ListItem>
          <ListItem>
            <TextField
               floatingLabelText='Current sample ID'
               value={currentSampleId} />
            <RaisedButton label='Audit' onClick={this.openDialog} />
          </ListItem>
        </List>
      </Card>
    );
  }
}

AuditInfo.PropTypes = {};

const mapStateToProps = state => {
  const { audit } = state;
  return { audit };
};

const mapDispatchToProps = dispatch => {
  return {};
};


export default connect(mapStateToProps, mapDispatchToProps)(AuditInfo);
