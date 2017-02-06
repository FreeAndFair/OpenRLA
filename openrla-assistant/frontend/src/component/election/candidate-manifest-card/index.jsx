import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';
import { remote } from 'electron';

import _ from 'lodash';

import submitCandidateManifest from '../../../action/submitCandidateManifest';

import ManifestCard from '../manifest-card';

import CandidateManifest from './candidate-manifest';


const CandidateManifestCard = ({ candidates }) => {
  const manifestEl = <CandidateManifest candidates={candidates} />;
  const title = 'Candidate Manifest';
  const subtitle = 'Upload or view the manifest for the candidates in the election.';
  const viewDisabled = _.isEmpty(candidates);

  return (
    <ManifestCard
       manifestEl={manifestEl}
       title={title}
       subtitle={subtitle}
       submitManifest={submitCandidateManifest}
       viewDisabled={viewDisabled} />
  );
};

CandidateManifestCard.propTypes = {
  candidates: PropTypes.array.isRequired,
};

const mapStateToProps = state => {
  const { election: { contests } } = state;

  const candidates = _.flatten(_.map(contests, c => _.values(c.candidates)));

  return { candidates };
};

export default connect(mapStateToProps)(CandidateManifestCard);
