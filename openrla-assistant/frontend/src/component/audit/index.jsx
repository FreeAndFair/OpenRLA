import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import NoAudit from './NoAudit';


const Audit = ({ audit }) => {
  if (_.isEmpty(audit)) {
    return <NoAudit />;
  }

  return <NoAudit />;
};

const mapStateToProps = ({ audit }) => ({ audit });

export default connect(mapStateToProps)(Audit);
