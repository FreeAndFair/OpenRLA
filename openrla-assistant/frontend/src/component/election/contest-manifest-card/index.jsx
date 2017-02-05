import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';

import _ from 'lodash';

import submitContestManifest from '../../../action/submitContestManifest';

import ManifestCard from '../manifest-card';

import ContestManifest from './contest-manifest';


const ContestManifestCard = ({ contests }) => {
  const manifestEl = <ContestManifest contests={contests} />;
  const title = 'Contest Manifest';
  const subtitle = 'Upload or view the manifest for the contests in the election.';
  const viewDisabled = _.isEmpty(contests);

  return (
    <ManifestCard
       manifestEl={manifestEl}
       title={title}
       subtitle={subtitle}
       submitManifest={submitContestManifest}
       viewDisabled={viewDisabled} />
  );
};

ContestManifestCard.propTypes = {
  contests: PropTypes.object.isRequired,
};

export default ContestManifestCard;
