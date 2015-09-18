'use strict';

/* Nav Controller */

(function(){

    var samplerNavControllers = angular.module('samplerApp.controllers.nav', []);

    samplerNavControllers.controller('NavCtrl', ['$log', '$route', '$scope',
      function ($log, $route, $scope) {

        // TODO: consider calculating the class once on route change.
        //
        // For example, can return "about.html" or "main.html".
        $scope.navButtonClass = function (buttonPage) {
            // $route.current and $route.current.templateUrl are undefined
            // in certain circumstances.
            var url = $route && $route.current && $route.current.templateUrl;
            if (!url) {
                return {};
            }
            var urlParts = url.split('/');
            var currentPage = urlParts[urlParts.length - 1];
            return {'active': (currentPage === buttonPage)};
        };
    }]);

})();
