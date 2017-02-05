import _ from 'lodash';


const cb = (acc, o) => {
  acc[o.id] = o;
  return acc;
};


export default contests => _.reduce(contests, cb, {});
