import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import HomeButton from './home-button';


const Archive = () => (
  <div>
    <h1>Audit Archives</h1>
    <HomeButton />
  </div>
);

export default connect()(Archive);
