import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardTitle,
  CardText
} from 'material-ui/Card';
import { List, ListItem } from 'material-ui/List';
import DatePicker from 'material-ui/DatePicker';
import RaisedButton from 'material-ui/RaisedButton';
import TextField from 'material-ui/TextField';


class DefineAudit extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <Card>
        <CardTitle title='Define Audit' />
        <CardText>
          Define a new audit.
        </CardText>
        <List>
          <ListItem>
            <TextField />
          </ListItem>
          <ListItem>
            <TextField />
          </ListItem>
          <ListItem>
            <TextField />
          </ListItem>
        </List>
      </Card>
    );
  }
}

export default connect()(DefineAudit);
