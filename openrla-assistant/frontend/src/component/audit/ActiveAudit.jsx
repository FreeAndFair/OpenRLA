import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import AuditInfo from './AuditInfo';
import AuditMarkList from './AuditMarkList';


class ActiveAudit extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div>
        <AuditInfo />
        <AuditMarkList />
      </div>
    );
  }
}

ActiveAudit.PropTypes = {};

const mapStateToProps = state => {
  return {};
};

const mapDispatchToProps = dispatch => {
  return {};
};


export default connect(mapStateToProps, mapDispatchToProps)(ActiveAudit);
