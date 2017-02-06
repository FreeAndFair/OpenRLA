import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import _ from 'lodash';

import submitContestManifest from '../../../action/submitContestManifest';

import FileUploadCard from '../FileUploadCard';

import ContestManifest from './ContestManifest';


const ContestManifestCard = ({
  contests,
  uploadDisabled,
  viewDisabled,
}) => {
  const uploadedDataEl = <ContestManifest contests={contests} />;
  const title = 'Contest Manifest';
  const subtitle = 'Upload or view the manifest for the contests in the election.';

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

const mapStateToProps = state => {
  const { election } = state;
  const { contests, id } = election;

  const uploadDisabled = !id || !_.isEmpty(contests);
  const viewDisabled = _.isEmpty(contests);

  return {
    contests,
    uploadDisabled,
    viewDisabled,
  };
};

export default connect(mapStateToProps)(ContestManifestCard);
