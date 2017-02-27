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

import ContestOutcome from './ContestOutcome';


class Outcomes extends React.Component {
  constructor(props) {
    super(props);

    this.state = {};
  }

  render() {
    const { election } = this.props;
    const { contests } = election;

    const contestOutcomes = _.map(contests, c => (
      <ContestOutcome key={c.id} contest={c} electionId={election.id} />
    ));

    return (
      <Card>
        <CardTitle title='Contest outcomes' />
        <CardText>
          <List>
            {contestOutcomes}
          </List>
        </CardText>
      </Card>
    );
  }
}

Outcomes.propTypes = {
  election: PropTypes.object.isRequired,
};

const mapStateToProps = ({ election }) => ({ election });

const mapDispatchToProps = dispatch => ({});

export default connect(mapStateToProps, mapDispatchToProps)(Outcomes);
