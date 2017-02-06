import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';

import _ from 'lodash';

import submitContestManifest from '../../../action/submitContestManifest';

import FileUploadCard from '../FileUploadCard';

import ContestManifest from './ContestManifest';


const ContestManifestCard = ({ contests }) => {
  const uploadedDataEl = <ContestManifest contests={contests} />;
  const title = 'Contest Manifest';
  const subtitle = 'Upload or view the manifest for the contests in the election.';
  const uploadDisabled = !_.isEmpty(contests);
  const viewDisabled = _.isEmpty(contests);

  return (
    <FileUploadCard
       multiSelections={false}
       uploadedDataEl={uploadedDataEl}
       title={title}
       subtitle={subtitle}
       submitFiles={submitContestManifest}
       uploadDisabled={uploadDisabled}
       viewDisabled={viewDisabled} />
  );
};

ContestManifestCard.propTypes = {
  contests: PropTypes.object.isRequired,
};

export default ContestManifestCard;
