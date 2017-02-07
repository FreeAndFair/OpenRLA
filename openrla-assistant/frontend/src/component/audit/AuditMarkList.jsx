import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import { Card } from 'material-ui/Card';

import AuditMark from './AuditMark';


class AuditMarkList extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    // temp
    const m = (bId, cId1, cId2) => ({
      ballotId: bId,
      marks: {
        1001: { contestId: 1001, candidateId: cId1 },
        1003: { contestId: 1003, candidateId: cId2 },
      },
    });

    // synthetic data
    const marks = {
      1: m(1, 1, 6),
      2: m(2, 1, 6),
      3: m(3, 2, 6),
      4: m(4, 1, 6),
      5: m(5, 3, 6),
      6: m(6, 1, 6),
    };

    const makeMark = mark => (
      <AuditMark
         key={mark.ballotId}
         ballotMark={mark} />
    );
    const auditMarks = _.map(marks, makeMark);

    return (
      <Card>
        {auditMarks}
      </Card>
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
