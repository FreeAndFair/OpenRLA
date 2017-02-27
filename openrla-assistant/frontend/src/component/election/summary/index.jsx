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

import addElection from 'action/addElection';
import resetElection from 'action/resetElection';
import saveElection from 'action/saveElection';


class ElectionSummary extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      date: null,
      title: "",
    };

    [
      'addElection',
      'onDateChange',
      'onTitleChange',
      'saveElection',
    ].forEach(m => {
      this[m] = this[m].bind(this);
    });
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

    let resetElectionButton;
    let saveOrAddButton;
    if (electionDefined) {
      resetElectionButton = <RaisedButton label='Reset' onClick={this.props.resetElection} />;
      saveOrAddButton = <RaisedButton label='Save' onClick={this.saveElection} />;
    } else {
      saveOrAddButton = <RaisedButton label='Add' onClick={this.addElection} />;
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
                   onChange={this.onTitleChange}
                   value={form.title}
                   id='formTitle'
                   ref='formTitle' />
              </ListItem>
              <ListItem secondaryText='Election date'>
                <DatePicker
                   onChange={this.onDateChange}
                   value={form.date && new Date(form.date)}
                   id='formDate'
                   ref='formDate' />
              </ListItem>
            </List>
          </CardText>
          {resetElectionButton}
          {saveOrAddButton}
        </Card>
      </div>
    );
  }
}

ElectionSummary.propTypes = {
  election: PropTypes.object.isRequired,
  addElection: PropTypes.func.isRequired,
  resetElection: PropTypes.func.isRequired,
  saveElection: PropTypes.func.isRequired,
};

const mapDispatchToProps = dispatch => ({
  addElection: data => dispatch(addElection(data)),
  resetElection: () => dispatch(resetElection()),
  saveElection: election => dispatch(saveElection(election)),
});

export default connect(null, mapDispatchToProps)(ElectionSummary);
