module.exports = ['$scope', 'Entries',
		  function($scope, Entries) {
		      $scope.selectedEntry = null;
		      $scope.entries = null;

		      $scope.selectEntry = function(e) {
			  $scope.selectedEntry = e;
		      };
		      
		      Entries.query(function(data) {
			  $scope.entries = data;
			  if (data.length > 0) {
			      $scope.selectedEntry = data[0];
			  }
		      });
		  }]
