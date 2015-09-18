// Karma configuration
// http://karma-runner.github.io/0.12/config/configuration-file.html
// Generated on 2014-08-09 using
// generator-karma 0.8.3

module.exports = function(config) {
  'use strict';

  config.set({
    // enable / disable watching file and executing tests whenever any file changes
    autoWatch: true,

    // base path, that will be used to resolve files and exclude
    basePath: '../',

    // testing framework to use (jasmine/mocha/qunit/...)
    frameworks: ['jasmine'],

    preprocessors: {
      // Convert JSON files to JS strings so our tests can be synchronous.
      //
      // Specifically, this exposes our JSON test-case data as a string
      // value in the Javascript object window.__html__,
      // provided we included the path to the JSON file in "files" below.
      // Without doing this, we would need to access the JSON file
      // in our test suite asynchronously using AJAX, which is slower
      // and not obviously permitted in Angular's Karma/Jasmine test
      // framework, since $http is mocked by default.
      'bower_components/rivest-sampler-tests/*.json': ['html2js']
    },

    // list of files / patterns to load in the browser
    files: [
      'bower_components/angular/angular.js',
      'bower_components/angular-mocks/angular-mocks.js',
      'app/scripts/**/*.js',
      'bower_components/big-int/src/BigInt.js',
      'bower_components/jsSHA/src/sha256.js',
      'bower_components/rivest-sampler-tests/tests.json',
      'test/mock/**/*.js',
      'test/unit/**/*.js'
    ],

    // list of files / patterns to exclude
    exclude: [],

    // web server port
    port: 8080,

    // Start these browsers, currently available:
    // - Chrome
    // - ChromeCanary
    // - Firefox
    // - Opera
    // - Safari (only Mac)
    // - PhantomJS
    // - IE (only Windows)
    browsers: [
//      'Chrome'
      'PhantomJS'
    ],

    // Which plugins to enable
    plugins: [
      'karma-html2js-preprocessor',
      'karma-phantomjs-launcher',
      'karma-jasmine'
    ],

    // Continuous Integration mode
    // if true, it capture browsers, run tests and exit
    singleRun: false,

    colors: true,

    // level of logging
    // possible values: LOG_DISABLE || LOG_ERROR || LOG_WARN || LOG_INFO || LOG_DEBUG
    logLevel: config.LOG_INFO,

    // Uncomment the following lines if you are using grunt's server to run the tests
    // proxies: {
    //   '/': 'http://localhost:9000/'
    // },
    // URL root prevent conflicts with the site root
    // urlRoot: '_karma_'
  });
};
