import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';

import ElectionInfoCard from './ElectionInfoCard';
import AuditInfoCard from './AuditInfoCard';


const Home = () => (
  <div id="start">
    <ElectionInfoCard />
    <AuditInfoCard />
  </div>
);

export default Home;
