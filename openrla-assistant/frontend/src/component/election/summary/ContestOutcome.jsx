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
import RaisedButton from 'material-ui/RaisedButton';
import TextField from 'material-ui/TextField';

import submitOutcomes from 'action/submitOutcomes';


class ContestOutcome extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      formOutcomes: {},
    };

    [
      'isFormValid',
      'onShareChange',
      'resetOutcomes',
      'saveOutcomes',
    ].forEach(m => {
      this[m] = this[m].bind(this);
    });
  }

  onShareChange(candidate) {
    const onChange = (_e, share) => {
      const candidateId = candidate.id;

      const newState = _.merge({}, this.state);
      newState.formOutcomes[candidateId] = { candidateId, share };

      this.setState(newState);
    };

    return onChange.bind(this);
  }

  isFormValid() {
    return true;
  }

  resetOutcomes() {
    const formOutcomes = this.props.outcomes;

    this.setState({ formOutcomes: {} });
  }

  saveOutcomes() {
    const shares = _.map(this.state.formOutcomes, o => ({
      id: o.candidateId,
      share: Number.parseFloat(o.share),
    }));
    const data = {
      id: this.props.contest.id,
      shares,
    };

    this.props.submitOutcomes(data);
  }

  render() {
    const { contest } = this.props;
    const { candidates } = contest;

    const displayOutcomes = _.merge({}, this.props.outcomes, this.state.formOutcomes);

    const candidatesById = _.sortBy(candidates, 'id');
    const candidateOutcomes = _.map(candidatesById, c => {
      const displayOutcome = displayOutcomes[c.id];
      return (
        <ListItem key={c.id} >
          <TextField
             floatingLabelText='Candidate Id'
             value={c.id} />
          <TextField
             floatingLabelText='Description'
             value={c.description} />
          <TextField
             onChange={this.onShareChange(c)}
             floatingLabelText='Candidate Share'
             value={displayOutcome.share} />
        </ListItem>
      );
    });

    return (
      <Card>
        <CardText>
          <List>
            <ListItem>
              <TextField
                 floatingLabelText='Contest ID'
                 value={contest.id} />
              <TextField
                 floatingLabelText='Description'
                 value={contest.description} />
            </ListItem>
            <ListItem>
              <List>
                {candidateOutcomes}
              </List>
            </ListItem>
          </List>
          <RaisedButton
             label='Save'
             onClick={this.saveOutcomes} />
          <RaisedButton
             label='Reset'
             onClick={this.resetOutcomes} />
        </CardText>
      </Card>
    );
  }
}

ContestOutcome.propTypes = {
  contest: PropTypes.object.isRequired,
  electionId: PropTypes.number.isRequired,
  outcomes: PropTypes.object.isRequired,
};

const mapStateToProps = (_state, props) => {
  let { contest } = props;

  const makeOutcome = candidate => ({
    candidateId: candidate.id,
    share: candidate.share,
  });

  let outcomes = _.map(contest.candidates, makeOutcome);
  outcomes = _.keyBy(outcomes, 'candidateId');
  return { outcomes };
};

const mapDispatchToProps = (dispatch, props) => {
  const { electionId } = props;
  return {
    submitOutcomes: outcomes => submitOutcomes(electionId, outcomes)(dispatch),
  };
};

export default connect(mapStateToProps, mapDispatchToProps)(ContestOutcome);
