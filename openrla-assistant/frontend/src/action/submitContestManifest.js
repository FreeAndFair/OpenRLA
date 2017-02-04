import { fetch, submit } from '../util';


export default filePath => (dispatch, getState) => {
  const electionId = getState().election.id;
  const data = {
    electionId,
    vendor: 'dominion',
    type: 'contest',
    filePath,
  };
  submit('/manifest', data)
    .then(manifest => {
      dispatch({
        type: 'UPDATE_CONTEST_MANIFEST',
        manifest,
      });
      fetch(`/election/${electionId}/contest`)
        .then(contests => dispatch({
          type: 'UPDATE_CONTESTS',
          contests,
        }));
    });
}
