import _ from 'lodash';


const keyById = a => _.keyBy(a, o => o.id);


export default ballots => keyById(ballots);
