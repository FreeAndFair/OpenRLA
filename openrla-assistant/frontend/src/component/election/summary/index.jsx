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


const emptyForm = () => ({
  date: null,
  edited: false,
  title: "",
});


class ElectionSummary extends React.Component {
  constructor(props) {
    super(props);

    this.state = emptyForm();

    [
      'addElection',
      'onDateChange',
      'onTitleChange',
      'resetElection',
      'saveElection',
    ].forEach(m => {
      this[m] = this[m].bind(this);
    });
  }

  addElection() {
    this.props.addElection(this.formData());
  }

  resetElection() {
    this.setState(emptyForm());
    this.props.resetElection();
  }

  saveElection() {
    const { id, active } = this.props.election;
    const { date, title } = this.formData();
    const data = { id, title, date, active };

    this.props.saveElection(data);
    this.setState(emptyForm());
  }

  onDateChange(_e, dateOb) {
    const date = `${dateOb}`;
    const newState = _.merge({}, this.formData(), { date, edited: true });
    this.setState(newState);
  }

  onTitleChange(e) {
    const title = e.target.value;
    const newState = _.merge({}, this.formData(), { edited: true, title });
    this.setState(newState);
  }

  formData() {
    const { date, title } = this.props.election;
    const state = { date, title };

    if (this.state.edited) {
      return _.merge(emptyForm(), state, this.state);
    } else {
      return _.merge(emptyForm(), state);
    }
  }

  render() {
    const { election, addElection, saveElection } = this.props;

    const electionDefined = !_.isNil(election.id);

    let resetElectionButton;
    let saveOrAddButton;
    if (electionDefined) {
      resetElectionButton = <RaisedButton label='Reset' onClick={this.resetElection} />;
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
            <div style={{ display: 'flex', justifyContent: 'space-around' }}>
              <TextField
                 floatingLabelText='Election title'
                 onChange={this.onTitleChange}
                 value={form.title}
                 id='formTitle'
                 ref='formTitle' />
              <DatePicker
                 floatingLabelText='Election date'
                 onChange={this.onDateChange}
                 value={form.date && new Date(form.date)}
                 id='formDate'
                 ref='formDate' />
            </div>
          </CardText>
          <div style={{ display: 'flex' }}>
            <div style={{ margin: '5px' }}>
              {resetElectionButton}
            </div>
            <div style={{ margin: '5px' }}>
              {saveOrAddButton}
            </div>
          </div>
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
