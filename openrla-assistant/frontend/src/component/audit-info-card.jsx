import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardActions,
  CardTitle,
  CardText
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';

import setPage from '../action/setPage';
import isAuditActive from '../selector/is-audit-active';


const AuditInfoCard = ({ audit, active, navigateAudit }) => {
  let status;

  if (active) {
    status = <div>Audit is ongoing.</div>;
  } else {
    status = <div>No active audit.</div>;
  }

  return (
    <Card>
      <CardTitle
         title="Audit"
         subtitle="Start or continue" />
      <CardText>
        Start or continue an audit for the current election,
        including customization of risk-limit.
      </CardText>
      <CardActions>
        <RaisedButton label="Audit" onClick={navigateAudit} />
      </CardActions>
    </Card>
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
  navigateAudit: () => dispatch(setPage('audit')),
});


export default connect(mapStateToProps, mapDispatchToProps)(AuditInfoCard);
