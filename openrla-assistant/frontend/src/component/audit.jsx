import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';


const Audit = () => (
  <div>
    <h1>Current audit</h1>
  </div>
);

export default connect()(Audit);
