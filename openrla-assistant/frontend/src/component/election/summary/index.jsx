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

import ContestOutcomes from './ContestOutcomes.jsx';

import addElection from 'action/addElection';
import saveElection from 'action/saveElection';


class ElectionSummary extends React.Component {
  constructor(props) {
    super(props);

    this.state = {};
  }

  addElection() {
    this.props.addElection(this.formData());
  }

  saveElection() {
    const { id, active } = this.props.election;
    const { date, title } = this.formData();
    const data = { id, title, date, active };

    this.props.saveElection(data);
  }

  onDateChange(_, dateOb) {
    const date = `${dateOb}`;
    this.setState({ date });
  }

  onTitleChange(e) {
    const title = e.target.value;
    this.setState({ title });
  }

  formData() {
    const { date, title } = this.props.election;
    const form = { date, title };
    Object.assign(form, this.state);
    return form;
  }

  render() {
    const { election, addElection, saveElection } = this.props;

    const electionDefined = !_.isNil(election.id);

    let button;
    if (electionDefined) {
      button = <RaisedButton label='Save' onClick={this.saveElection.bind(this)} />;
    } else {
      button = <RaisedButton label='Add' onClick={this.addElection.bind(this)} />;
    }

    const form = this.formData();

    return (
      <div>
        <Card>
          <CardTitle title='Election summary' />
          <CardText>
            <List>
              <ListItem secondaryText='Election title'>
                <TextField
                   onChange={this.onTitleChange.bind(this)}
                   value={form.title}
                   id='formTitle'
                   ref='formTitle' />
              </ListItem>
              <ListItem secondaryText='Election date'>
                <DatePicker
                   onChange={this.onDateChange.bind(this)}
                   value={form.date && new Date(form.date)}
                   id='formDate'
                   ref='formDate' />
              </ListItem>
            </List>
          </CardText>
          {button}
        </Card>
        <ContestOutcomes />
      </div>
    );
  }
}

ElectionSummary.propTypes = {
  election: PropTypes.object.isRequired,
  addElection: PropTypes.func.isRequired,
  saveElection: PropTypes.func.isRequired,
};

const mapDispatchToProps = dispatch => ({
  addElection: data => dispatch(addElection(data)),
  saveElection: election => dispatch(saveElection(election)),
});

export default connect(null, mapDispatchToProps)(ElectionSummary);
