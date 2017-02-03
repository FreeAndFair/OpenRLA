import _ from 'lodash';


export default election => {
  if (_.isNil(election.id)) return false;
  if (_.isEmpty(election.contests)) return false;
  // TODO: check candidate existence, &c

  return true;
}
