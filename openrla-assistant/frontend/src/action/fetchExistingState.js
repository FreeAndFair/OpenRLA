import { fetch } from '../util';

import fetchActiveAudit from './fetchActiveAudit';
import fetchAuditMarks from './fetchAuditMarks';
import fetchAuditSample from './fetchAuditSample';
import fetchBallots from './fetchBallots';
import fetchContests from './fetchContests';


export default () => dispatch => {
  fetch('/election/active')
    .then(election => dispatch({
      type: 'UPDATE_ELECTION',
      election,
    }))
    .then(({ election: { id } }) => {
      fetchBallots(id)(dispatch);
      fetchContests(id)(dispatch);
      fetchActiveAudit()(dispatch)
        .then(audit => {
          const { id } = audit;
          fetchAuditMarks(id)(dispatch);
          fetchAuditSample(id)(dispatch);
        });
    })
    .catch(console.error);
}
