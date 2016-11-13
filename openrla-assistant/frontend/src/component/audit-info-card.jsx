import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import isAuditActive from '../selector/is-audit-active';


const AuditInfoCard = ({ audit, active, navigateAudit }) => {
  const navigateButton = (
    <button onClick={navigateAudit}>View active audit</button>
  );

  let status;

  if (active) {
    status = <div>Audit is ongoing.</div>;
  } else {
    status = <div>No active audit.</div>;
  }

  return (
    <div>
      {status}
      {navigateButton}
    </div>
  );
};

AuditInfoCard.propTypes = {
  audit: PropTypes.object.isRequired,
  active: PropTypes.bool.isRequired,
  navigateAudit: PropTypes.func.isRequired,
};

const mapStateToProps = ({ audit }) => ({
  audit,
  active: isAuditActive(audit),
});

const mapDispatchToProps = dispatch => ({
  navigateAudit: () => dispatch({ type: 'NAVIGATE_AUDIT' }),
});


export default connect(mapStateToProps, mapDispatchToProps)(AuditInfoCard);
