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


const ElectionSummary = ({ election }) => {
  let cardText;
  let subtitle;

  if (_.isEmpty(election)) {
    subtitle = 'No election defined';
  } else {
    cardText = (
      <CardText>
        <List>
          <ListItem
             primaryText={election.title}
             secondaryText='Election title' />
          <ListItem
             primaryText={election.date}
             secondaryText='Election date' />
        </List>
      </CardText>
    );
  }

  return (
    <Card>
      <CardTitle
         title='Election summary'
         subtitle={subtitle} />
      {cardText}
    </Card>
  );
};

ElectionSummary.propTypes = {
  election: PropTypes.object.isRequired,
};

export default ElectionSummary;
