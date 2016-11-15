import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';


const Audit = ({ audit }) => {


  return (
    <div>
      <h1>Current audit</h1>
    </div>
  );
}

const mapStateToProps = ({ audit }) => ({ audit });

export default connect(mapStateToProps)(Audit);
