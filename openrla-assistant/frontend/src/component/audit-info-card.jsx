import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import isAuditActive from '../selector/is-audit-active';


const AuditInfoCard = ({audit, active}) => {
  if (active) {
    return (
      <div>Audit is ongoing.</div>
    );
  } else {
    return (
      <div>No active audit.</div>
    );
  }
};

AuditInfoCard.propTypes = {
  audit: PropTypes.object.isRequired,
  active: PropTypes.bool.isRequired,
};

const mapStateToProps = ({ audit, election }) => ({
  audit,
  active: isAuditActive(audit),
});

export default connect(mapStateToProps)(AuditInfoCard);
