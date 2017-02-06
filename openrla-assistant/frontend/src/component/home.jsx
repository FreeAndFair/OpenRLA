import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';

import ElectionInfoCard from './home/ElectionInfoCard';
import ArchiveInfoCard from './home/ArchiveInfoCard';
import AuditInfoCard from './home/AuditInfoCard';


const Home = () => (
  <div id="start">
    <ElectionInfoCard />
    <AuditInfoCard />
    <ArchiveInfoCard />
  </div>
);

export default Home;
