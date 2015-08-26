module.exports = ['$scope', 'Entries',
		  function($scope, Entries) {
		      $scope.entries = Entries.query(function() {
			  console.log("Got a response");
		      });
		  }]
