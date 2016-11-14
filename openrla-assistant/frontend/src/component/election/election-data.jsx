import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';

import _ from 'lodash';


const ElectionData = ({ election }) => {
  const { title } = election;

  return (
    <div>
      <h1>{title}</h1>
    </div>
  );
};

ElectionData.propTypes = {
  election: PropTypes.object.isRequired,
};

export default ElectionData;
