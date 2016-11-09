import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';


const HomeButton = ({ navigateHome }) => (
  <button onClick={navigateHome}>Home</button>
);

const mapDispatchToProps = dispatch => ({
  navigateHome: () => dispatch({ type: 'NAVIGATE_HOME' }),
});

export default connect(null, mapDispatchToProps)(HomeButton);
