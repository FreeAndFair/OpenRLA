import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';

import ElectionInfoCard from './ElectionInfoCard';
import AuditInfoCard from './AuditInfoCard';


const homeCardStyle = {
  width: '400px',
  height: '400px',
  margin: '50px',
  padding: '20px',
};


const Home = () => (
  <div id="start" style={{ display: 'flex', justifyContent: 'center' }}>
    <div>
      <ElectionInfoCard style={homeCardStyle} />
    </div>
    <div>
      <AuditInfoCard style={homeCardStyle} />
    </div>
  </div>
);

export default Home;
