import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';


class AuditMark extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div>Audit mark</div>
    );
  }
}

AuditMark.PropTypes = {
  mark: PropTypes.object.isRequired,
};

const mapStateToProps = state => {
  return {};
};

const mapDispatchToProps = dispatch => {
  return {};
};


export default connect(mapStateToProps, mapDispatchToProps)(AuditMark);
