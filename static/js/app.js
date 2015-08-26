var angular = require('angular');
require('angular-resource');
require('angular-route');
require('angular-ui-bootstrap');
require('angular-ui-bootstrap-tpls');

var app = angular.module('fsApp', ['ngResource', 'ngRoute', 'ui.bootstrap', 'ui.bootstrap.tpls']);

app.config(['$locationProvider', '$routeProvider', function($locationProvider, $routeProvider) {
    $locationProvider.html5Mode(true).hashPrefix('!');
}]);

require('./controller/index.js');
require('./services/index.js');
