import { fetch } from '../util';

import pivotAuditMarks from 'selector/pivotAuditMarks';


export default id => dispatch => {
  fetch(`/audit/${id}/marks`)
    .then(marks => dispatch({
      type: 'UPDATE_AUDIT_MARKS',
      marks: pivotAuditMarks(marks),
    }))
    .catch(console.error);
}
