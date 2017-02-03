import React from 'react';
import ReactDOM from 'react-dom';
import Dialog from 'material-ui/Dialog';
import FlatButton from 'material-ui/FlatButton';
import RaisedButton from 'material-ui/RaisedButton';
import _ from 'lodash';

import ContestManifest from './contest-manifest';


const styles = {
  radioButton: {
    marginTop: 16,
  },
};

export default class ViewContestManifestButton extends React.Component {
  constructor(props) {
    super(props);
    this.state = { open: false };
  }

  handleOpen() {
    this.setState({ open: true });
  };

  handleClose() {
    this.setState({ open: false });
  };

  render() {
    const closeButton = (
      <FlatButton
         label="Close"
         primary={true}
         onTouchTap={this.handleClose.bind(this)} />
    );
    const actions = [closeButton];
    const disabled = _.isEmpty(this.props.contests);

    return (
      <RaisedButton
         disabled={disabled}
         label="View"
         onTouchTap={this.handleOpen.bind(this)} >
        <Dialog
           title="Contest Manifest"
           actions={actions}
           modal={false}
           open={this.state.open}
           onRequestClose={this.handleClose.bind(this)}
           autoScrollBodyContent={true} >
          <ContestManifest contests={this.props.contests} />
        </Dialog>
      </RaisedButton>
    );
  }
}
