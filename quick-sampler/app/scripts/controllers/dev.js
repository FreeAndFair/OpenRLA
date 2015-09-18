'use strict';

/* Dev Controller */

(function(){

    var samplerDevControllers = angular.module('samplerApp.controllers.dev', []);

    // This controller is for convenience when developing locally.
    //
    // The controller populates the form with sensible values and then
    // submits it.  This lets one visually inspect form output without
    // needing to repopulate the form after each code change.
    samplerDevControllers.controller('DevCtrl', ['$log', '$route', '$scope',
      function ($log, $route, $scope) {

        var defaultInputs = {
            debug: true,
            sampleCount: 12,
            seed: 'snowman: \u2603',
            smallestItem: 1,
            totalCount: 1000
        };

        angular.extend($scope.form.input, defaultInputs);

        $scope.form.submit();

    }]);

})();
