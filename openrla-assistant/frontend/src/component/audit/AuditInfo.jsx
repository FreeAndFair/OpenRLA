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

    return (
      <Card>
        <List>
          <ListItem secondaryText='Election ID'>
            <TextField value={audit.electionId} />
          </ListItem>
          <ListItem secondaryText='Audit ID'>
            <TextField value={audit.id} />
          </ListItem>
          <ListItem secondaryText='Date'>
            <DatePicker value={new Date(audit.date)} />
          </ListItem>
          <ListItem secondaryText='Risk limit'>
            <TextField value={audit.riskLimit} />
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
