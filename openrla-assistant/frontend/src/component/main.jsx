import React from 'react';
import ReactDOM from 'react-dom';

import Component from '../component';


export default class Main extends Component {
  constructor() {
    super();
    this._bind('increment', 'reset');
  }

  render() {
    const state = this.props.store.getState();
    return (
      <div>
        <h1>OpenRLA Assistant</h1>
        <h2>Free & Fair</h2>
        <div>Count: {state.count}</div>
        <div>
          <button onClick={this.increment}>Add</button>
          <button onClick={this.reset}>Reset</button>
        </div>
      </div>
    );
  }

  increment() {
    this.props.store.dispatch({ type: 'INCREMENT' });
  }

  reset() {
    this.props.store.dispatch({ type: 'RESET' });
  }
}
