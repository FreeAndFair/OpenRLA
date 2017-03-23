import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import {
  Card,
  CardActions,
  CardHeader,
  CardTitle,
  CardText
} from 'material-ui/Card';
import RaisedButton from 'material-ui/RaisedButton';

import setPage from '../../action/setPage';
import isAuditActive from '../../selector/isAuditActive';


const cardActionsStyle = {
  display: 'flex',
  justifyContent: 'center',
  paddingTop: '50px',
};


const AuditInfoCard = ({ audit, active, navigateAudit, style }) => {
  let status;

  if (active) {
    status = <div>Audit is ongoing.</div>;
  } else {
    status = <div>No active audit.</div>;
  }

  return (
    <Card style={style}>
      <CardHeader>
        <CardTitle
           title="Audit"
           subtitle="Start or continue" />
      </CardHeader>
      <CardText>
        Start or continue an audit for the current election,
        including customization of risk-limit.
      </CardText>
      <CardActions style={cardActionsStyle}>
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
