import { fetch, submit } from '../util';

import fetchActiveAudit from './fetchActiveAudit';
import fetchAuditMarks from './fetchAuditMarks';
import fetchAuditSample from './fetchAuditSample';


export default (id, data) => dispatch => {
  submit(`/audit/${id}/marks`, data)
    .then(() => {
      fetchActiveAudit(id)(dispatch);
      fetchAuditMarks(id)(dispatch);
      fetchAuditSample(id)(dispatch);
    });
};
