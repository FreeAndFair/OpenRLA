// Webpack config for an Electron "renderer" process.
path = require('path');

module.exports = {
  devtool: 'source-map',
  entry: 'index.js',
  output: {
    filename: './dist/app.dist.js',
  },
  module: {
    loaders: [
      {
        test: /\.jsx?$/,
        exclude: /node_modules/,
        loader: 'babel-loader',
        query: {
          presets: ['react', 'es2015']
        }
      }
    ]
  },
  resolve: {
    // Don't require the use of these file extensions in import paths
    extensions: ['', '.js', '.jsx'],
    root: path.resolve(__dirname, 'src'),
  },
  target: 'electron-renderer',
}
