'use strict';

/* Application */

// Uncomment the below to see the "omitted" part of an error message
// in the console.  This is a work-around for an issue that was fixed
// in Chrome version 37.  See here for more info:
// http://stackoverflow.com/questions/22527436/how-to-see-whats-omitted-in-chrome-debug-console
// window.onerror = function (errorMsg, url, lineNumber, columnNumber, errorObject) {
//     if (errorObject && /<omitted>/.test(errorMsg)) {
//         console.error('Full exception message: ' + errorObject.message);
//     }
// };

(function(){

  /**
   * @ngdoc overview
   * @name samplerApp
   * @description
   * # samplerApp
   *
   * Main module of the application.
   */
  var samplerApp = angular.module('samplerApp', [
      'ngRoute',
      'samplerApp.controllers.dev',
      'samplerApp.controllers.form',
      'samplerApp.controllers.nav',
      'samplerApp.directives'
  ]);

  samplerApp.config(['$logProvider',
    function($logProvider) {
      $logProvider.debugEnabled(false);
    }]);

  samplerApp.config(['$routeProvider',
    function($routeProvider) {
      $routeProvider.
        when('/', {
          templateUrl: 'templates/partials/main.html'
        }).
        // This route is a convenience when developing locally.
        when('/dev', {
          templateUrl: 'templates/partials/main.html',
          controller: 'DevCtrl'
        }).
        when('/about', {
          templateUrl: 'templates/partials/about.html'
        }).
        otherwise({
          redirectTo: '/'
        });
    }]);

    samplerApp.run(['$log',
      function($log) {
        $log.log('starting app');
        $log.debug('debug enabled');
    }]);


})();
