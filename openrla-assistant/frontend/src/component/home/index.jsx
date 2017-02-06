import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';

import ElectionInfoCard from './ElectionInfoCard';
import ArchiveInfoCard from './ArchiveInfoCard';
import AuditInfoCard from './AuditInfoCard';


const Home = () => (
  <div id="start">
    <ElectionInfoCard />
    <AuditInfoCard />
    <ArchiveInfoCard />
  </div>
);

export default Home;
