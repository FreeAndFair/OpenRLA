import _ from 'lodash';


export default (audit, election) => {
  const ld = _;

  const { riskLimit } = audit;
  const { contests } = election;
  const auditContests = _.map(audit.contests, c => contests[c.id]);

  const marginToASN = margin =>
          2 * Math.log(1 / riskLimit) / (margin * margin);

  const contestToMargin = contest => {
    const { candidates } = contest;
    const winner = _.maxBy(_.toArray(candidates), c => c.share);
    if (!winner) return;
    const losers = _.omit(candidates, winner.id);
    const bestLoser = _.maxBy(_.toArray(losers), c => c.share);
    const winnerShare = winner.share;
    const loserShare = bestLoser.share;
    const fracMargin = winnerShare - loserShare;
    return fracMargin;
  };

  const allMargins = _.map(auditContests, contestToMargin);
  const maxMargin = _.max(allMargins);

  return Math.ceil(marginToASN(maxMargin));
};
