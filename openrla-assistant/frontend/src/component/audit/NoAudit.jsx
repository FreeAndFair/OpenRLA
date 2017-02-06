import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardActions,
  CardText,
  CardTitle,
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';


class NoAudit extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <Card>
        <CardTitle title='Audit' />
        <CardText>
          No active audit.
        </CardText>
        <CardActions>
          <RaisedButton
             label='Start'
             onClick={console.log} />
          <RaisedButton
             disabled={true}
             label='Archive'
             onClick={console.log} />
        </CardActions>
      </Card>
    );
  }
}

export default connect()(NoAudit);
