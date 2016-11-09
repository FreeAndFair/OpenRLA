import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';

import ElectionInfoCard from './election-info-card';
import ArchiveInfoCard from './archive-info-card';
import AuditInfoCard from './audit-info-card';


const Home = () => (
  <div id="start">
    <ElectionInfoCard />
    <AuditInfoCard />
    <ArchiveInfoCard />
  </div>
);

export default Home;
