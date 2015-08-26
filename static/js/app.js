var angular = require('angular');
require('angular-resource');
require('angular-route');

var app = angular.module('fsApp', ['ngResource', 'ngRoute']);

app.config(['$locationProvider', '$routeProvider', function($locationProvider, $routeProvider) {
    $locationProvider.html5Mode(true).hashPrefix('!');
}]);

require('./controller/index.js');
require('./services/index.js');
