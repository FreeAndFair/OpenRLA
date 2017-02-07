import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';


class AuditInfo extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div>Active audit info</div>
    );
  }
}

AuditInfo.PropTypes = {};

const mapStateToProps = state => {
  return {};
};

const mapDispatchToProps = dispatch => {
  return {};
};


export default connect(mapStateToProps, mapDispatchToProps)(AuditInfo);
