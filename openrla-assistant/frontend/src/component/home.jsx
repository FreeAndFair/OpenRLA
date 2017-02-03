import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';

import ElectionInfoCard from './home/election-info-card';
import ArchiveInfoCard from './home/archive-info-card';
import AuditInfoCard from './home/audit-info-card';


const Home = () => (
  <div id="start">
    <ElectionInfoCard />
    <AuditInfoCard />
    <ArchiveInfoCard />
  </div>
);

export default Home;
