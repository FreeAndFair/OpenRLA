import { fetch, submit } from '../util';

import fetchAuditSample from './fetchAuditSample';


export default data => dispatch => {
  submit('/audit', data).then(audit => {
    fetch(`/audit/${audit.id}`)
      .then(audit => dispatch({
        type: 'UPDATE_AUDIT',
        audit,
      }))
      .then(fetchAuditSample(audit.id)(dispatch));
  });
};
