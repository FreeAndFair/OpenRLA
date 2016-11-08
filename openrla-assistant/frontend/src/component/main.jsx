import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import ElectionInfoCard from './election-info-card';
import AuditInfoCard from './audit-info-card';


const Main = () => (
  <div>
    <h1>OpenRLA Assistant</h1>
    <h2>Free & Fair</h2>
    <ElectionInfoCard />
    <AuditInfoCard />
  </div>
);

export default Main;
