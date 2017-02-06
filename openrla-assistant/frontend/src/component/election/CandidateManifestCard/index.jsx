import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';
import { remote } from 'electron';

import _ from 'lodash';

import submitCandidateManifest from '../../../action/submitCandidateManifest';

import FileUploadCard from '../FileUploadCard';

import CandidateManifest from './CandidateManifest';


const CandidateManifestCard = ({
  candidates,
  uploadDisabled,
  viewDisabled,
}) => {
  const uploadedDataEl = <CandidateManifest candidates={candidates} />;
  const title = 'Candidate Manifest';
  const subtitle = 'Upload or view the manifest for the candidates in the election.';

  return (
    <FileUploadCard
       multiSelections={false}
       uploadedDataEl={uploadedDataEl}
       title={title}
       subtitle={subtitle}
       submitFiles={submitCandidateManifest}
       uploadDisabled={uploadDisabled}
       viewDisabled={viewDisabled} />
  );
};

CandidateManifestCard.propTypes = {
  candidates: PropTypes.array.isRequired,
  uploadDisabled: PropTypes.bool.isRequired,
  viewDisabled: PropTypes.bool.isRequired,
};

const mapStateToProps = state => {
  const { election } = state;
  const { contests, id } = election;

  const candidates = _.flatten(_.map(contests, c => _.values(c.candidates)));
  const uploadDisabled = !id || _.isEmpty(contests) || !_.isEmpty(candidates);
  const viewDisabled = _.isEmpty(candidates);

  return {
    candidates,
    uploadDisabled,
    viewDisabled,
  };
};

export default connect(mapStateToProps)(CandidateManifestCard);
