import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import { Card } from 'material-ui/Card';
import { List, ListItem } from 'material-ui/List';

import AuditMark from './AuditMark';


class AuditMarkList extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { marks } = this.props;
    // TODO: `key` should really be `sampleId`, not just the array index
    const makeMark = (mark, ix) => (
      <ListItem key={ix}>
        <AuditMark ballotMark={mark} />
      </ListItem>
    );
    const auditMarks = _.map(marks, makeMark);

    return (
      <List>
        {auditMarks}
      </List>
    );
  }
}

AuditMarkList.PropTypes = {};

const mapStateToProps = state => {
  const { audit: { marks } } = state;
  return { marks };
};

const mapDispatchToProps = dispatch => {
  return {};
};


export default connect(mapStateToProps, mapDispatchToProps)(AuditMarkList);
