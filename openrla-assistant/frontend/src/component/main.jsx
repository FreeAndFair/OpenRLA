import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';


const Main = ({count, increment, reset}) => (
  <div>
    <h1>OpenRLA Assistant</h1>
    <h2>Free & Fair</h2>
    <div>Count: {count}</div>
    <div>
      <button onClick={increment}>Add</button>
      <button onClick={reset}>Reset</button>
    </div>
  </div>
);

Main.propTypes = {
  count: PropTypes.number.isRequired,
  increment: PropTypes.func.isRequired,
  reset: PropTypes.func.isRequired,
};

const mapStateToProps = ({ count }) => ({ count });

const mapDispatchToProps = dispatch => ({
  increment: () => dispatch({ type: 'INCREMENT' }),
  reset: () => dispatch({ type: 'RESET' }),
});

export default connect(mapStateToProps, mapDispatchToProps)(Main);
