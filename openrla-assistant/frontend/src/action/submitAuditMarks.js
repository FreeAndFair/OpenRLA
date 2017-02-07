import { fetch, submit } from '../util';

import fetchAuditMarks from './fetchAuditMarks';
import fetchAuditSample from './fetchAuditSample';


export default (id, data) => dispatch => {
  submit(`/audit/${id}/marks`, data)
    .then(() => {
      fetchAuditMarks(id)(dispatch);
      fetchAuditSample(id)(dispatch);
    });
};
