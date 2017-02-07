import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import AuditMark from './AuditMark';


class AuditMarkList extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    const marks = ['a', 'b', 'c'];

    const mkMark = mark => <AuditMark mark={mark} />;

    const auditMarks = React.Children.map(marks, mkMark);

    return (
      <div>
        {auditMarks}
      </div>
    );
  }
}

AuditMarkList.PropTypes = {};

const mapStateToProps = state => {
  return {};
};

const mapDispatchToProps = dispatch => {
  return {};
};


export default connect(mapStateToProps, mapDispatchToProps)(AuditMarkList);
