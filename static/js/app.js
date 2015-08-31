var angular = require('angular');
require('angular-animate');
require('angular-resource');
require('angular-route');
require('angular-sanitize');
require('angular-toastr');
require('angular-ui-bootstrap-tpls');

var app = angular.module('fsApp', ['ngAnimate',
				   'ngResource',
				   'ngRoute',
				   'ngSanitize',
				   'toastr',
				   'ui.bootstrap']);

app.config(['$locationProvider', '$routeProvider', function($locationProvider, $routeProvider) {
    $locationProvider.html5Mode(true).hashPrefix('!');
}]);

require('./controller/index.js');
require('./services/index.js');
