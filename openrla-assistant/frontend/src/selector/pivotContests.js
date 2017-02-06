import _ from 'lodash';


const keyById = a => _.keyBy(a, o => o.id);


export default contests => {
  const cb = c => {
    c.candidates = keyById(c.candidates);
    return c;
  };
  const result = keyById(_.map(contests, cb));
  return result;
};
