import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';


const Outcomes = ({ election }) => {
  return (
    <div>
      Election outcomes.
    </div>
  );
};

Outcomes.propTypes = {
  election: PropTypes.object.isRequired,
};

const mapStateToProps = ({ election }) => ({ election });

export default connect(mapStateToProps)(Outcomes);
