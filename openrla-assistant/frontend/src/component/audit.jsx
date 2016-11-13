import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import HomeButton from './home-button';


const Audit = () => (
  <div>
    <h1>Current audit</h1>
    <HomeButton />
  </div>
);

export default connect()(Audit);
