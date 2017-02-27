import React, { PropTypes } from 'react';
import ReactDOM from 'react-dom';
import { connect } from 'react-redux';

import { Tab, Tabs } from 'material-ui/Tabs';

import Audit from './audit';
import Home from './home';
import Election from './election';

import setPage from '../action/setPage';


const Main = ({ page, archive, changeTab }) => {
  return (
    <Tabs value={page} onChange={changeTab}>
      <Tab label='Home' value='home'>
        <Home />
      </Tab>
      <Tab label='Election' value='election'>
        <Election />
      </Tab>
      <Tab label='Audit' value='audit'>
        <Audit />
      </Tab>
    </Tabs>
  );
};

Main.PropTypes = {
  archive: PropTypes.array.isRequired,
  page: PropTypes.string.isRequired,
  changeTab: PropTypes.func.isRequired,
};

const mapStateToProps = ({ page, archive }) => ({ page, archive });

const mapDispatchToProps = dispatch => ({
  changeTab: page => dispatch(setPage(page)),
});

export default connect(mapStateToProps, mapDispatchToProps)(Main);
